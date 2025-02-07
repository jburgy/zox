const std = @import("std");
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
const run = exec.run;
const value = @import("value.zig");
comptime {
    _ = @import("emit.zig");
}

const Code = std.ArrayList(u8);
const N = @sizeOf(value.Box);

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
    tokens: []const Token,
    nodes: []const Node,
    locals: std.ArrayListUnmanaged(u32),
    code: Code,

    pub fn init(allocator: Allocator, tokens: []const Token, nodes: []const Node, locals: []u32) Compiler {
        return .{
            .tokens = tokens,
            .nodes = nodes,
            .locals = std.ArrayListUnmanaged(u32).initBuffer(locals),
            .code = Code.init(allocator),
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.code.deinit();
    }

    fn getIndex(self: Compiler, name: []const u8) ?usize {
        const tokens = self.tokens;
        for (self.locals.items, 0..) |token, i|
            if (mem.eql(u8, tokens[token].src, name)) return i;
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
                try self.code.appendSlice(&mem.toBytes(num));
                effect += 1;
            },
            .BANG_EQUAL, .EQUAL_EQUAL, .MINUS, .PLUS, .SLASH, .STAR, .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => {
                if (count == 1) { // -x ⇒ 0 - x
                    try self.code.append(opcode("box"));
                    try self.code.appendSlice(&mem.toBytes(value.box(0.0)));
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
                self.code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(self.code.items.len - offset));
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
                self.code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(self.code.items.len - offset));
            },
            .VAR => {
                if (count > 1) {
                    effect += try self.compile(self.nodes[node + 2].node);
                } else {
                    try self.code.append(opcode("box"));
                    try self.code.appendSlice(&mem.toBytes(value.box({})));
                    effect += 1;
                }
                // https://discord.com/channels/605571803288698900/1333444838691180554
                self.locals.appendAssumeCapacity(self.nodes[node + 1].node);
            },
            .EQUAL => {
                const name = self.tokens[self.nodes[self.nodes[node + 1].node].head.token].src;
                if (self.getIndex(name)) |index| {
                    effect += try self.compile(self.nodes[node + 2].node);
                    try self.code.append(opcode("set"));
                    try self.code.append(@truncate(index));
                    effect -= 1;
                } else {
                    std.debug.print("use of undeclared identifier '{s}'.", .{name});
                    return error.UndeclaredIdentifier;
                }
            },
            .IDENTIFIER => {
                const name = token.src;
                if (self.getIndex(name)) |index| {
                    try self.code.append(opcode("get"));
                    try self.code.append(@truncate(index));
                    effect += 1;
                } else {
                    std.debug.print("use of undeclared identifier '{s}'.", .{name});
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
                    self.code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(self.code.items.len - offset));
                    effect += try self.compile(self.nodes[node + 3].node);
                    self.code.replaceRangeAssumeCapacity(other, N, &mem.toBytes(self.code.items.len - other));
                } else {
                    self.code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(self.code.items.len - offset));
                }
            },
            .WHILE => {
                assert(count == 2);
                const start = self.code.items.len;
                effect += try self.compile(self.nodes[node + 1].node);
                try self.code.append(opcode("jif"));
                const offset = self.code.items.len;
                try self.code.appendNTimes(0xAA, N);
                effect -= 1;
                effect += try self.compile(self.nodes[node + 2].node);
                try self.code.append(opcode("ebb"));
                try self.code.appendSlice(&mem.toBytes(self.code.items.len - start));
                self.code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(self.code.items.len - offset));
            },
            .FOR => {
                assert(count == 4);
                effect += try self.compile(self.nodes[node + 1].node);
                const start = self.code.items.len;
                effect += try self.compile(self.nodes[node + 2].node);
                try self.code.append(opcode("jif"));
                const offset = self.code.items.len;
                try self.code.appendNTimes(0xAA, N);
                effect -= 1;
                effect += try self.compile(self.nodes[node + 3].node);
                const n = try self.compile(self.nodes[node + 4].node);
                try self.code.appendNTimes(opcode("pop"), @abs(n));
                effect -= n;
                try self.code.append(opcode("ebb"));
                try self.code.appendSlice(&mem.toBytes(self.code.items.len - start));
                self.code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(self.code.items.len - offset));
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
                var locals: [std.math.maxInt(u8)]u32 = undefined;
                var child = Compiler.init(self.code.allocator, self.tokens, self.nodes, &locals);
                defer child.deinit();
                for (child.nodes[node + 1 .. node + count]) |name|
                    child.locals.appendAssumeCapacity(name.head.token);
                _ = try child.compile(self.nodes[node + count].node);
                try child.code.append(opcode("ret"));
                // install function address (past "box value; jmp offset") in parent scope
                self.locals.appendAssumeCapacity(self.nodes[node + 1].node);
                try self.code.append(opcode("box"));
                try self.code.appendSlice(&mem.toBytes(self.code.items.len + 2 * N + 1));
                effect += 1;
                // skip over function body
                try self.code.append(opcode("jmp"));
                try self.code.appendSlice(&mem.toBytes(child.code.items.len + N));
                try self.code.appendSlice(child.code.items);
            },
            .RIGHT_PAREN => {
                try self.code.append(opcode("box"));
                try self.code.appendSlice(&mem.toBytes(value.box({})));
                const index = node + 1;
                for (self.nodes[index .. index + count]) |child|
                    effect += try self.compile(child.node);
                assert(effect == count);
                try self.code.append(opcode("call"));
                try self.code.append(@truncate(count));
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
    const one = mem.toBytes(value.box(1.0));
    const expected = .{opcode("box")} ++ one ++ .{opcode("box")} ++ one ++ .{ opcode("add"), opcode("end") };

    var locals: [std.math.maxInt(u8)]u32 = undefined;
    var compiler = Compiler.init(testing.allocator, tokens[0..], nodes[0..], &locals);
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
    var compiler = Compiler.init(allocator, tokens, nodes.items, &locals);
    defer compiler.deinit();
    const n = try compiler.compile(root);

    var values = exec.allocate_values(std.math.maxInt(u8));
    var frames = exec.allocate_frames(64);
    frames.appendAssumeCapacity(.{});
    try run(compiler.code.items, &values, &frames);

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
}

test {
    _ = tokenize;
    _ = parse;
    _ = @import("emit.zig");
    _ = @import("evaluate.zig");
    _ = value;
    _ = exec;
}
