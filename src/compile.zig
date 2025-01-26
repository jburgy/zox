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
const Index = exec.Index;
const Stack = exec.Stack;
const Values = exec.Values;
const opcode = exec.opcode;
const run = exec.run;
const value = @import("value.zig");
comptime {
    _ = @import("emit.zig");
}

const Indices = std.SinglyLinkedList(std.StringHashMap(u24));
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

fn find(indices: *Indices, name: []const u8) ?Index {
    var depth: u8 = 0;
    var it = indices.first;
    while (it) |n| : ({
        it = n.next;
        depth += 1;
    }) {
        if (n.data.get(name)) |index|
            return .{ .depth = depth, .index = index };
    }
    return null;
}

pub fn compile(program: *std.ArrayList(u8), indices: *Indices, tokens: []const Token, nodes: []const Node, node: usize) !void {
    const token = tokens[nodes[node].head.token];
    const count = nodes[node].head.count;
    switch (token.tag) {
        .EOF => {
            const index = node + 1;
            for (nodes[index .. index + count]) |arg|
                try compile(program, indices, tokens, nodes, arg.node);
            try program.append(opcode("end"));
        },
        .NUMBER => {
            const num = try std.fmt.parseFloat(value.Box, token.src);
            try program.append(opcode("box"));
            try program.appendSlice(&mem.toBytes(num));
        },
        .BANG_EQUAL, .EQUAL_EQUAL, .MINUS, .PLUS, .SLASH, .STAR, .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => {
            if (count == 1) { // -x ⇒ 0 - x
                try program.append(opcode("box"));
                try program.appendSlice(&mem.toBytes(value.box(0.0)));
                try compile(program, indices, tokens, nodes, nodes[node + 1].node);
            } else {
                assert(count == 2);
                try compile(program, indices, tokens, nodes, nodes[node + 1].node);
                try compile(program, indices, tokens, nodes, nodes[node + 2].node);
            }
            try program.append(greedy.get(token.tag).?);
        },
        .AND => { // if (a) a else b
            assert(count == 2);
            try compile(program, indices, tokens, nodes, nodes[node + 1].node);
            try program.append(opcode("dup"));
            try program.append(opcode("jif"));
            try program.appendNTimes(0xAA, N); // https://ziglang.org/documentation/master/#undefined
            const offset = program.items.len;
            try program.append(opcode("pop"));
            try compile(program, indices, tokens, nodes, nodes[node + 2].node);
            program.replaceRangeAssumeCapacity(offset - N, N, &mem.toBytes(program.items.len - offset));
        },
        .OR => { // if (!a) a else b
            assert(count == 2);
            try compile(program, indices, tokens, nodes, nodes[node + 1].node);
            try program.append(opcode("dup"));
            try program.append(opcode("not"));
            try program.append(opcode("jif"));
            try program.appendNTimes(0xAA, N);
            const offset = program.items.len;
            try program.append(opcode("pop"));
            try compile(program, indices, tokens, nodes, nodes[node + 2].node);
            program.replaceRangeAssumeCapacity(offset - N, N, &mem.toBytes(program.items.len - offset));
        },
        .VAR => {
            if (indices.popFirst()) |first| {
                var map = first.data;
                const name = tokens[nodes[node + 1].node].src;
                const index: u24 = @truncate(map.count());
                const gop = try map.getOrPut(name);
                if (!gop.found_existing) gop.value_ptr.* = index;
                if (count > 1) {
                    try compile(program, indices, tokens, nodes, nodes[node + 2].node);
                    try program.append(opcode("set"));
                    try program.appendSlice(&mem.toBytes(Index{ .depth = 0, .index = index }));
                }
                first.data = map;
                indices.prepend(first);
            } else unreachable;
        },
        .EQUAL => {
            const name = tokens[nodes[nodes[node + 1].node].head.token].src;
            if (find(indices, name)) |index| {
                try compile(program, indices, tokens, nodes, nodes[node + 2].node);
                try program.append(opcode("set"));
                try program.appendSlice(&mem.toBytes(index));
            } else {
                std.debug.print("use of undeclared identifier '{s}'.", .{name});
                return error.UndeclaredIdentifier;
            }
        },
        .IDENTIFIER => {
            const name = token.src;
            if (find(indices, name)) |index| {
                try program.append(opcode("get"));
                try program.appendSlice(&mem.toBytes(index));
            } else {
                std.debug.print("use of undeclared identifier '{s}'.", .{name});
                return error.UndeclaredIdentifier;
            }
        },
        .IF => {
            assert(count == 2 or count == 3);
            try compile(program, indices, tokens, nodes, nodes[node + 1].node);
            try program.append(opcode("jif"));
            try program.appendNTimes(0xAA, N);
            const offset = program.items.len;
            try compile(program, indices, tokens, nodes, nodes[node + 2].node);
            if (count == 3) {
                try program.append(opcode("jmp"));
                try program.appendNTimes(0xAA, N);
                const other = program.items.len;
                program.replaceRangeAssumeCapacity(offset - N, N, &mem.toBytes(program.items.len - offset));
                try compile(program, indices, tokens, nodes, nodes[node + 3].node);
                program.replaceRangeAssumeCapacity(other - N, N, &mem.toBytes(program.items.len - other));
            } else {
                program.replaceRangeAssumeCapacity(offset - N, N, &mem.toBytes(program.items.len - offset));
            }
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
    const one = mem.toBytes(value.box(1.0));
    const expected = .{opcode("box")} ++ one ++ .{opcode("box")} ++ one ++ .{ opcode("add"), opcode("end") };
    var indices = Indices{};

    try compile(&actual, &indices, tokens[0..], nodes[0..], 5);
    try testing.expectEqualStrings(expected[0..], actual.items);
}

pub fn execute(allocator: Allocator, source: []const u8, values: *Values) !value.Box {
    const tokens = try tokenize.tokens(allocator, source);
    defer allocator.free(tokens);

    var nodes = parse.Nodes.init(allocator);
    defer nodes.deinit();

    const state = try parse.statements(&nodes, tokens, 0);
    std.debug.assert(state.token == tokens.len - 1);
    const root = state.node.node;
    std.debug.assert(root + 1 + nodes.items[root].head.count == nodes.items.len);

    var program = std.ArrayList(u8).init(allocator);
    defer program.deinit();
    var globals = Indices.Node{ .data = std.StringHashMap(u24).init(allocator) };
    defer globals.data.deinit();
    var indices = Indices{};
    indices.prepend(&globals);
    defer _ = indices.popFirst();
    try compile(&program, &indices, tokens, nodes.items, root);

    var stack = try Stack.initCapacity(allocator, 16);
    defer stack.deinit(allocator);
    run(allocator, &stack, program.items, values);

    std.debug.assert(stack.items.len == 1);
    return stack.pop();
}

test execute {
    const allocator = testing.allocator;
    var values = Values{};
    var data: [1]value.Box = undefined;
    var globals = Values.Node{ .data = &data };
    values.prepend(&globals);

    try testing.expectEqual(2.0, try execute(allocator, "1 + 1", &values));
    try testing.expectEqual(1.0, try execute(allocator, "var a = 1; a", &values));
    try testing.expectEqual(0.0, try execute(allocator, "var a = 0; if (a) a = 2; a", &values));
    try testing.expectEqual(2.0, try execute(allocator, "var a = 1; if (a) a = 2; a", &values));
}
