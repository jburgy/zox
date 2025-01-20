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
const Value = exec.Value;
const Values = exec.Values;
const Stack = exec.Stack;
const opcode = exec.opcode;
const run = exec.run;
comptime {
    _ = @import("emit.zig");
}

const zero = mem.zeroes([@sizeOf(f64)]u8);
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

pub fn compile(program: *std.ArrayList(u8), tokens: []const Token, nodes: []const Node, node: usize) !void {
    const token = tokens[nodes[node].head.token];
    const count = nodes[node].head.count;
    switch (token.tag) {
        .EOF => {
            const index = node + 1;
            for (nodes[index .. index + count]) |arg|
                try compile(program, tokens, nodes, arg.node);
            try program.append(opcode("end"));
        },
        .NUMBER => {
            const num = try std.fmt.parseFloat(f64, token.src);
            const buf: [@sizeOf(f64)]u8 = @bitCast(num);
            try program.append(opcode("num"));
            try program.appendSlice(buf[0..]);
        },
        .BANG_EQUAL, .EQUAL_EQUAL, .MINUS, .PLUS, .SLASH, .STAR, .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => {
            if (count == 1) { // -x ⇒ 0 - x
                try program.append(opcode("num"));
                try program.appendSlice(zero[0..]);
                try compile(program, tokens, nodes, nodes[node + 1].node);
            } else {
                assert(count == 2);
                try compile(program, tokens, nodes, nodes[node + 1].node);
                try compile(program, tokens, nodes, nodes[node + 2].node);
            }
            try program.append(greedy.get(token.tag).?);
        },
        .AND => { // if (a) a else b
            assert(count == 2);
            try compile(program, tokens, nodes, nodes[node + 1].node);
            try program.append(opcode("dup"));
            try program.append(opcode("jif"));
            try program.appendSlice(&zero);
            const offset = program.items.len;
            try program.append(opcode("pop"));
            try compile(program, tokens, nodes, nodes[node + 1].node);
            program.replaceRangeAssumeCapacity(offset, offset + zero.len, &mem.toBytes(program.items.len - offset));
        },
        .OR => { // if (!a) a else b
            assert(count == 2);
            try compile(program, tokens, nodes, nodes[node + 1].node);
            try program.append(opcode("dup"));
            try program.append(opcode("not"));
            try program.append(opcode("jif"));
            try program.appendSlice(&zero);
            const offset = program.items.len;
            try program.append(opcode("pop"));
            try compile(program, tokens, nodes, nodes[node + 1].node);
            program.replaceRangeAssumeCapacity(offset, offset + zero.len, &mem.toBytes(program.items.len - offset));
        },
        else => @panic("not supported"),
    }
}

test compile {
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
    var actual = std.ArrayList(u8).init(testing.allocator);
    defer actual.deinit();
    const one = mem.toBytes(@as(f64, 1.0));
    const expected = .{opcode("num")} ++ one ++ .{opcode("num")} ++ one ++ .{ opcode("add"), opcode("end") };

    try compile(&actual, tokens[0..], nodes[0..], 5);
    try testing.expectEqualStrings(expected[0..], actual.items);
}

pub fn execute(allocator: Allocator, source: []const u8) !Value {
    const tokens = try tokenize.tokens(allocator, source);
    defer allocator.free(tokens);

    var nodes = try parse.Nodes.initCapacity(allocator, 16);
    defer nodes.deinit(allocator);

    const state = try parse.statements(&nodes, allocator, tokens, 0);
    std.debug.assert(state.token == tokens.len - 1);
    const root = state.node.node;
    std.debug.assert(root + 1 + nodes.items[root].head.count == nodes.items.len);

    var program = std.ArrayList(u8).init(allocator);
    defer program.deinit();
    try compile(&program, tokens, nodes.items, root);

    var stack = try Stack.initCapacity(allocator, 16);
    defer stack.deinit(allocator);
    var values = Values{};
    try run(allocator, &stack, program.items, &values);

    std.debug.assert(stack.items.len == 1);
    return stack.pop();
}

test execute {
    const allocator = testing.allocator;
    try testing.expectEqual(
        Value{ .number = 2.0 },
        try execute(allocator, "1 + 1"),
    );
}
