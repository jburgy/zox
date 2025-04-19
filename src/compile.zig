const std = @import("std");
const math = std.math;
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;
const assert = std.debug.assert;
const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const parse = @import("parse.zig");
const Node = parse.Node;
const exec = @import("execute.zig");
const opcode = exec.opcode;
const value = @import("value.zig");

const Code = std.ArrayList(u8);
const N = @sizeOf(i32);

// See https://github.com/ziglang/zig/pull/20074
const greedy = init: {
    var temp = std.EnumMap(Token.Tag, u8){};
    temp.put(.BANG_EQUAL, opcode("neq"));
    temp.put(.EQUAL_EQUAL, opcode("eql"));
    temp.put(.MINUS, opcode("sub"));
    temp.put(.PLUS, opcode("add"));
    temp.put(.SLASH, opcode("div"));
    temp.put(.STAR, opcode("mul"));
    temp.put(.LESS, opcode("lt"));
    temp.put(.LESS_EQUAL, opcode("lte"));
    temp.put(.GREATER, opcode("gt"));
    temp.put(.GREATER_EQUAL, opcode("gte"));
    break :init temp;
};

const Compiler = struct {
    enclosing: ?*Compiler = null,
    tokens: []const Token,
    nodes: []const Node,
    locals: std.ArrayListUnmanaged(u32),
    upvalues: std.AutoArrayHashMap(u32, i8),
    code: Code,

    pub fn init(
        enclosing: ?*Compiler,
        allocator: Allocator,
        tokens: []const Token,
        nodes: []const Node,
        locals: []u32,
    ) Compiler {
        return .{
            .enclosing = enclosing,
            .tokens = tokens,
            .nodes = nodes,
            .locals = std.ArrayListUnmanaged(u32).initBuffer(locals),
            .upvalues = std.AutoArrayHashMap(u32, i8).init(allocator),
            .code = Code.init(allocator),
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.upvalues.deinit();
        self.code.deinit();
    }

    inline fn comeFrom(self: Compiler, offset: usize) void {
        mem.writeInt(
            i32,
            self.code.items[offset..][0..N],
            math.cast(i32, self.code.items.len - offset - N).?,
            .little,
        );
    }

    fn resolveLocal(self: Compiler, token: u24) ?u8 {
        const tokens = self.tokens;
        const name = tokens[token].src;
        for (self.locals.items, 0..) |t, i|
            if (mem.eql(u8, tokens[t].src, name)) return @intCast(i);
        return null;
    }

    fn addUpvalue(self: *Compiler, token: u24, index: i8) ?u8 {
        const count: u8 = @intCast(self.upvalues.count());
        return if (self.upvalues.putNoClobber(token, index)) |_| count else |_| null;
    }

    fn resolveUpValue(self: *Compiler, token: u24) ?u8 {
        if (self.enclosing) |enclosing| {
            if (self.resolveLocal(token)) |i| {
                const j: i8 = @intCast(i);
                return self.addUpvalue(token, -j);
            }
            if (enclosing.resolveUpValue(token)) |i|
                return self.addUpvalue(token, @intCast(i));
        }
        return null;
    }

    pub fn compile(self: *Compiler, node: usize) !isize {
        const token = self.tokens[self.nodes[node].head.token];
        const count = self.nodes[node].head.count;
        var effect: isize = 0;
        switch (token.tag) {
            .EOF => {
                const index = node + 1;
                for (self.nodes[index .. index + count]) |child|
                    effect += try self.compile(child.node);
                try self.code.append(opcode("end"));
            },
            .NUMBER => {
                const num = try std.fmt.parseFloat(value.Box, token.src);
                try self.code.append(opcode("box"));
                try self.code.writer().writeInt(usize, @bitCast(num), .little);
                effect += 1;
            },
            .BANG_EQUAL, .EQUAL_EQUAL, .MINUS, .PLUS, .SLASH, .STAR, .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => {
                if (count == 1) { // -x ⇒ 0 - x
                    try self.code.append(opcode("box"));
                    try self.code.writer().writeInt(usize, @bitCast(value.box(0.0)), .little);
                    effect += 1;
                    effect += try self.compile(self.nodes[node + 1].node);
                } else {
                    assert(count == 2);
                    effect += try self.compile(self.nodes[node + 1].node);
                    effect += try self.compile(self.nodes[node + 2].node);
                }
                assert(effect >= 2);
                try self.code.append(greedy.get(token.tag).?);
                effect -= 1;
            },
            .AND => { // if (a) a else b
                assert(count == 2);
                effect += try self.compile(self.nodes[node + 1].node);
                try self.code.append(opcode("dup"));
                try self.code.append(opcode("jif"));
                const offset = self.code.items.len;
                try self.code.appendNTimes(0xAA, N); // https://ziglang.org/documentation/master/#undefined
                try self.code.append(opcode("pop"));
                effect -= 1;
                effect += try self.compile(self.nodes[node + 2].node);
                self.comeFrom(offset);
            },
            .OR => { // if (!a) a else b
                assert(count == 2);
                effect += try self.compile(self.nodes[node + 1].node);
                try self.code.append(opcode("dup"));
                try self.code.append(opcode("not"));
                try self.code.append(opcode("jif"));
                const offset = self.code.items.len;
                try self.code.appendNTimes(0xAA, N);
                try self.code.append(opcode("pop"));
                effect -= 1;
                effect += try self.compile(self.nodes[node + 2].node);
                self.comeFrom(offset);
            },
            .VAR => {
                if (count > 1) {
                    effect += try self.compile(self.nodes[node + 2].node);
                } else {
                    try self.code.append(opcode("box"));
                    try self.code.writer().writeInt(usize, @bitCast(value.box({})), .little);
                    effect += 1;
                }
                // https://discord.com/channels/605571803288698900/1333444838691180554
                self.locals.appendAssumeCapacity(self.nodes[node + 1].node);
            },
            .EQUAL => {
                const name = self.nodes[self.nodes[node + 1].node].head.token;
                if (self.resolveLocal(name)) |index| {
                    effect += try self.compile(self.nodes[node + 2].node);
                    try self.code.append(opcode("set"));
                    try self.code.append(index);
                    effect -= 1;
                } else if (self.resolveUpValue(name)) |index| {
                    effect += try self.compile(self.nodes[node + 2].node);
                    try self.code.append(opcode("seu"));
                    try self.code.append(index);
                    effect -= 1;
                } else {
                    std.debug.print("use of undeclared identifier '{s}'.", .{self.tokens[name].src});
                    return error.UndeclaredIdentifier;
                }
            },
            .IDENTIFIER => {
                const name = self.nodes[node].head.token;
                if (self.resolveLocal(name)) |index| {
                    try self.code.append(opcode("get"));
                    try self.code.append(index);
                    effect += 1;
                } else if (self.resolveUpValue(name)) |index| {
                    try self.code.append(opcode("geu"));
                    try self.code.append(index);
                    effect += 1;
                } else {
                    std.debug.print("use of undeclared identifier '{s}'.", .{token.src});
                    return error.UndeclaredIdentifier;
                }
            },
            .IF => {
                assert(count == 2 or count == 3);
                effect += try self.compile(self.nodes[node + 1].node);
                try self.code.append(opcode("jif"));
                const offset = self.code.items.len;
                try self.code.appendNTimes(0xAA, N);
                effect -= 1;
                effect += try self.compile(self.nodes[node + 2].node);
                if (count == 3) {
                    try self.code.append(opcode("jmp"));
                    const other = self.code.items.len;
                    try self.code.appendNTimes(0xAA, N);
                    effect -= 1;
                    self.comeFrom(offset);
                    effect += try self.compile(self.nodes[node + 3].node);
                    self.comeFrom(other);
                } else {
                    self.comeFrom(offset);
                }
            },
            .WHILE => {
                assert(count == 2);
                const start = math.cast(i32, self.code.items.len).?;
                effect += try self.compile(self.nodes[node + 1].node);
                try self.code.append(opcode("jif"));
                const offset = self.code.items.len;
                try self.code.appendNTimes(0xAA, N);
                effect -= 1;
                effect += try self.compile(self.nodes[node + 2].node);
                try self.code.append(opcode("jmp"));
                try self.code.writer().writeInt(
                    i32,
                    start - math.cast(i32, self.code.items.len + N).?,
                    .little,
                );
                self.comeFrom(offset);
            },
            .FOR => {
                assert(count == 4);
                effect += try self.compile(self.nodes[node + 1].node);
                const start = math.cast(i32, self.code.items.len).?;
                effect += try self.compile(self.nodes[node + 2].node);
                try self.code.append(opcode("jif"));
                const offset = self.code.items.len;
                try self.code.appendNTimes(0xAA, N);
                effect -= 1;
                effect += try self.compile(self.nodes[node + 3].node);
                const n = try self.compile(self.nodes[node + 4].node);
                try self.code.appendNTimes(opcode("pop"), @abs(n));
                effect -= n;
                try self.code.append(opcode("jmp"));
                try self.code.writer().writeInt(
                    i32,
                    start - math.cast(i32, self.code.items.len + N).?,
                    .little,
                );
                self.comeFrom(offset);
            },
            .RIGHT_BRACE => {
                const depth = self.locals.items.len;
                const index = node + 1;
                for (self.nodes[index .. index + count]) |child|
                    effect += try self.compile(child.node);
                const n = self.locals.items.len - depth;
                for (0..n) |_| _ = self.locals.pop();
                try self.code.appendNTimes(opcode("pop"), n);
                effect -= @intCast(n);
            },
            .FUN => {
                assert(count >= 2);
                self.locals.appendAssumeCapacity(self.nodes[node + 1].node);
                var locals: [std.math.maxInt(u8)]u32 = undefined;
                var child = Compiler.init(self, self.code.allocator, self.tokens, self.nodes, &locals);
                defer child.deinit();
                for (child.nodes[node + 1 .. node + count]) |name|
                    child.locals.appendAssumeCapacity(name.head.token);
                _ = try child.compile(self.nodes[node + count].node);
                try child.code.append(opcode("ret"));
                // skip over function body and install function address in parent scope
                effect += 1;
                const n = child.upvalues.count();
                try self.code.append(opcode("fun"));
                try self.code.appendNTimes(0xAA, N);
                mem.writeInt(
                    i32,
                    self.code.items[self.code.items.len - N ..][0..N],
                    math.cast(i32, child.code.items.len + 2 + n).?,
                    .little,
                );
                try self.code.append(opcode("env"));
                try self.code.append(@intCast(n));
                for (child.upvalues.values()) |val|
                    try self.code.append(@bitCast(val));
                try self.code.appendSlice(child.code.items);
            },
            .RIGHT_PAREN => {
                const index = node + 1;
                for (self.nodes[index .. index + count]) |child|
                    effect += try self.compile(child.node);
                assert(effect == count);
                try self.code.append(opcode("call"));
                try self.code.append(@truncate(count));
                effect -= 1;
            },
            else => unreachable,
        }
        return effect;
    }
};

test Compiler {
    const source = "1 + 1";
    const tokens = [_]Token{
        .{ .tag = .NUMBER, .src = source[0..1] },
        .{ .tag = .PLUS, .src = source[2..3] },
        .{ .tag = .NUMBER, .src = source[4..5] },
        .{ .tag = .EOF, .src = source[5..5] },
    };
    const nodes = [_]Node{
        .{ .head = .{ .token = 0, .count = 0 } },
        .{ .head = .{ .token = 2, .count = 0 } },
        .{ .head = .{ .token = 1, .count = 2 } },
        .{ .node = 0 },
        .{ .node = 1 },
        .{ .head = .{ .token = 3, .count = 1 } },
        .{ .node = 2 },
    };
    const one = mem.toBytes(mem.nativeToLittle(value.Box, value.box(1.0)));
    const expected = .{opcode("box")} ++ one ++ .{opcode("box")} ++ one ++ .{ opcode("add"), opcode("end") };

    var locals: [std.math.maxInt(u8)]u32 = undefined;
    var compiler = Compiler.init(null, testing.allocator, tokens[0..], nodes[0..], &locals);
    defer compiler.deinit();
    const n = try compiler.compile(5);
    try testing.expectEqual(1, n);
    try testing.expectEqualStrings(expected[0..], compiler.code.items);
}

pub fn execute(allocator: Allocator, source: []const u8) !value.Box {
    const tokens = try tokenize.tokens(allocator, source);
    defer allocator.free(tokens);

    var nodes = parse.Nodes.init(allocator);
    defer nodes.deinit();

    const state = try parse.statements(&nodes, tokens, 0);
    std.debug.assert(state.token == tokens.len - 1);
    const root = state.node.node;
    std.debug.assert(root + 1 + nodes.items[root].head.count == nodes.items.len);

    var locals: [std.math.maxInt(u8)]u32 = undefined;
    var compiler = Compiler.init(null, allocator, tokens, nodes.items, &locals);
    defer compiler.deinit();
    const n = try compiler.compile(root);

    var values = try exec.Values.initCapacity(allocator, std.math.maxInt(u8));
    defer values.deinit();
    var frames = try exec.Frames.initCapacity(allocator, 64);
    defer frames.deinit();
    try exec.run(compiler.code.items, &values, &frames, allocator);

    std.debug.assert(values.items.len == n);
    return values.pop();
}

test execute {
    const allocator = testing.allocator;

    try testing.expectEqual(2.0, try execute(allocator, "1 + 1"));
    try testing.expectEqual(1.0, try execute(allocator, "var a = 1; a"));
    try testing.expectEqual(0.0, try execute(allocator, "var a = 0; if (a) a = 2; a"));
    try testing.expectEqual(2.0, try execute(allocator, "var a = 1; if (a) a = 2; a"));
    try testing.expectEqual(0.0, try execute(allocator, "var a = 1; while (a) a = a - 1; a"));
    try testing.expectEqual(
        10.0,
        try execute(allocator, "var sum = 0; for (var i = 0; i < 5; i = i + 1) sum = sum + i; sum"),
    );
    try testing.expectEqual(
        1.0,
        try execute(allocator, "fun f(x) { x + 1 }; f(0)"),
    );
    try testing.expectEqual(
        3.0,
        try execute(allocator, "fun f(x) { fun g(y) { x + y }; g; }; f(1)(2)"),
    );
}

test {
    _ = tokenize;
    _ = parse;
    _ = @import("emit.zig");
    _ = @import("evaluate.zig");
    _ = value;
    _ = exec;
}
