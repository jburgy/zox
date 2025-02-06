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
const Stack = exec.Stack;
const Values = exec.Values;
const opcode = exec.opcode;
const run = exec.run;
const value = @import("value.zig");
comptime {
    _ = @import("emit.zig");
}

const Indices = std.StringArrayHashMap(u8);
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
    locals: std.StringArrayHashMap(void),
    code: std.ArrayList(u8),

    pub fn init(allocator: Allocator, tokens: []const Token, nodes: []const Node) Compiler {
        return .{
            .tokens = tokens,
            .nodes = nodes,
            .locals = std.StringArrayHashMap(void).init(allocator),
            .code = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.locals.deinit();
        self.code.deinit();
    }

    pub fn compile(self: *Compiler, node: usize, params: ?[]const Node) !isize {
        const token = self.tokens[self.nodes[node].head.token];
        const count = self.nodes[node].head.count;
        var effect: isize = 0;
        switch (token.tag) {
            .EOF => {
                const index = node + 1;
                for (self.nodes[index .. index + count]) |child|
                    effect += try self.compile(child.node, null);
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
                    effect += try self.compile(self.nodes[node + 1].node, null);
                } else {
                    assert(count == 2);
                    effect += try self.compile(self.nodes[node + 1].node, null);
                    effect += try self.compile(self.nodes[node + 2].node, null);
                }
                assert(effect >= 2);
                try self.code.append(greedy.get(token.tag).?);
                effect -= 1;
            },
            .AND => { // if (a) a else b
                assert(count == 2);
                effect += try self.compile(self.nodes[node + 1].node, null);
                try self.code.append(opcode("dup"));
                try self.code.append(opcode("jif"));
                const offset = self.code.items.len;
                try self.code.appendNTimes(0xAA, N); // https://ziglang.org/documentation/master/#undefined
                try self.code.append(opcode("pop"));
                effect -= 1;
                effect += try self.compile(self.nodes[node + 2].node, null);
                self.code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(self.code.items.len - offset));
            },
            .OR => { // if (!a) a else b
                assert(count == 2);
                effect += try self.compile(self.nodes[node + 1].node, null);
                try self.code.append(opcode("dup"));
                try self.code.append(opcode("not"));
                try self.code.append(opcode("jif"));
                const offset = self.code.items.len;
                try self.code.appendNTimes(0xAA, N);
                try self.code.append(opcode("pop"));
                effect -= 1;
                effect += try self.compile(self.nodes[node + 2].node, null);
                self.code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(self.code.items.len - offset));
            },
            .VAR => {
                if (count > 1) {
                    effect += try self.compile(self.nodes[node + 2].node, null);
                } else {
                    try self.code.append(opcode("box"));
                    try self.code.appendSlice(&mem.toBytes(value.box({})));
                    effect += 1;
                }
                // https://discord.com/channels/605571803288698900/1333444838691180554
                const map = &self.locals;
                const name = self.tokens[self.nodes[node + 1].node].src;
                try map.putNoClobber(name, {});
            },
            .EQUAL => {
                const name = self.tokens[self.nodes[self.nodes[node + 1].node].head.token].src;
                if (self.locals.getIndex(name)) |index| {
                    effect += try self.compile(self.nodes[node + 2].node, null);
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
                if (self.locals.getIndex(name)) |index| {
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
                effect += try self.compile(self.nodes[node + 1].node, null);
                try self.code.append(opcode("jif"));
                const offset = self.code.items.len;
                try self.code.appendNTimes(0xAA, N);
                effect -= 1;
                effect += try self.compile(self.nodes[node + 2].node, null);
                if (count == 3) {
                    try self.code.append(opcode("jmp"));
                    const other = self.code.items.len;
                    try self.code.appendNTimes(0xAA, N);
                    effect -= 1;
                    self.code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(self.code.items.len - offset));
                    effect += try self.compile(self.nodes[node + 3].node, null);
                    self.code.replaceRangeAssumeCapacity(other, N, &mem.toBytes(self.code.items.len - other));
                } else {
                    self.code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(self.code.items.len - offset));
                }
            },
            .WHILE => {
                assert(count == 2);
                const start = self.code.items.len;
                effect += try self.compile(self.nodes[node + 1].node, null);
                try self.code.append(opcode("jif"));
                const offset = self.code.items.len;
                try self.code.appendNTimes(0xAA, N);
                effect -= 1;
                effect += try self.compile(self.nodes[node + 2].node, null);
                try self.code.append(opcode("ebb"));
                try self.code.appendSlice(&mem.toBytes(self.code.items.len - start));
                self.code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(self.code.items.len - offset));
            },
            .FOR => {
                assert(count == 4);
                effect += try self.compile(self.nodes[node + 1].node, null);
                const start = self.code.items.len;
                effect += try self.compile(self.nodes[node + 2].node, null);
                try self.code.append(opcode("jif"));
                const offset = self.code.items.len;
                try self.code.appendNTimes(0xAA, N);
                effect -= 1;
                effect += try self.compile(self.nodes[node + 3].node, null);
                const n = try self.compile(self.nodes[node + 4].node, null);
                try self.code.appendNTimes(opcode("pop"), @abs(n));
                effect -= n;
                try self.code.append(opcode("ebb"));
                try self.code.appendSlice(&mem.toBytes(self.code.items.len - start));
                self.code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(self.code.items.len - offset));
            },
            .RIGHT_BRACE => {
                const depth = self.locals.count();
                if (params) |parms| {
                    // function prologue (prime new scope with function parameters)
                    try self.locals.ensureUnusedCapacity(@truncate(parms.len));
                    for (parms) |param|
                        self.locals.putAssumeCapacityNoClobber(self.tokens[param.head.token].src, {});
                }
                const index = node + 1;
                for (self.nodes[index .. index + count]) |child|
                    effect += try self.compile(child.node, params);
                const n = self.locals.count() - depth;
                for (0..n) |_| _ = self.locals.pop();
                try self.code.appendNTimes(opcode("pop"), n);
                effect -= @intCast(n);
            },
            .FUN => {
                assert(count >= 2);
                // install function address in scope
                const map = &self.locals;
                const name = self.tokens[self.nodes[node + 1].node].src;
                try map.putNoClobber(name, {});
                try self.code.append(opcode("box"));
                const start = self.code.items.len;
                try self.code.appendNTimes(0xAA, N);
                // skip over function body
                try self.code.append(opcode("jmp"));
                const body = self.code.items.len;
                try self.code.appendNTimes(0xAA, N);
                self.code.replaceRangeAssumeCapacity(start, N, &mem.toBytes(self.code.items.len));
                const parms = self.nodes[node + 2 .. node + count];
                const side_effect = try self.compile(self.nodes[node + count].node, parms);
                std.debug.print("side_effect = {d}, parms.len = {d}\n", .{ side_effect, parms.len });
                assert(side_effect + @as(isize, @intCast(parms.len)) == 1);
                try self.code.append(opcode("ret"));
                try self.code.append(@truncate(parms.len));
                self.code.replaceRangeAssumeCapacity(body, N, &mem.toBytes(self.code.items.len - body));
            },
            .RIGHT_PAREN => {
                try self.code.append(opcode("box"));
                try self.code.appendSlice(&mem.toBytes(value.box({})));
                for (0..count) |i|
                    effect += try self.compile(self.nodes[node + count - i].node, null);
                assert(effect == count);
                try self.code.append(opcode("call"));
                effect -= 1;
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

    var compiler = Compiler.init(testing.allocator, tokens[0..], nodes[0..]);
    defer compiler.deinit();
    const n = try compiler.compile(5, null);
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

    var compiler = Compiler.init(allocator, tokens, nodes.items);
    defer compiler.deinit();
    const n = try compiler.compile(root, null);

    var stack = try Stack.initCapacity(allocator, 16);
    defer stack.deinit();
    try run(compiler.code.items, &stack);

    std.debug.assert(stack.items.len == n);
    return stack.pop();
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
    // try testing.expectEqual(
    //     1.0,
    //     try execute(allocator, "fun f(x) { x + 1 }; f(0)"),
    // );
}

test {
    _ = tokenize;
    _ = parse;
    _ = @import("emit.zig");
    _ = @import("evaluate.zig");
    _ = value;
    _ = exec;
}
