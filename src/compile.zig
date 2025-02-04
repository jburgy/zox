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

const Map = std.StringHashMap(u24);
const Indices = std.SinglyLinkedList(Map);
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

fn compile(
    code: *std.ArrayList(u8),
    indices: *Indices,
    tokens: []const Token,
    nodes: []const Node,
    node: usize,
    params: ?[]const Node,
) !isize {
    const token = tokens[nodes[node].head.token];
    const count = nodes[node].head.count;
    var effect: isize = 0;
    switch (token.tag) {
        .EOF => {
            const index = node + 1;
            for (nodes[index .. index + count]) |child|
                effect += try compile(code, indices, tokens, nodes, child.node, null);
            try code.append(opcode("end"));
        },
        .NUMBER => {
            const num = try std.fmt.parseFloat(value.Box, token.src);
            try code.append(opcode("box"));
            try code.appendSlice(&mem.toBytes(num));
            effect += 1;
        },
        .BANG_EQUAL, .EQUAL_EQUAL, .MINUS, .PLUS, .SLASH, .STAR, .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => {
            if (count == 1) { // -x ⇒ 0 - x
                try code.append(opcode("box"));
                try code.appendSlice(&mem.toBytes(value.box(0.0)));
                effect += 1;
                effect += try compile(code, indices, tokens, nodes, nodes[node + 1].node, null);
            } else {
                assert(count == 2);
                effect += try compile(code, indices, tokens, nodes, nodes[node + 1].node, null);
                effect += try compile(code, indices, tokens, nodes, nodes[node + 2].node, null);
            }
            assert(effect >= 2);
            try code.append(greedy.get(token.tag).?);
            effect -= 1;
        },
        .AND => { // if (a) a else b
            assert(count == 2);
            effect += try compile(code, indices, tokens, nodes, nodes[node + 1].node, null);
            try code.append(opcode("dup"));
            try code.append(opcode("jif"));
            const offset = code.items.len;
            try code.appendNTimes(0xAA, N); // https://ziglang.org/documentation/master/#undefined
            try code.append(opcode("pop"));
            effect -= 1;
            effect += try compile(code, indices, tokens, nodes, nodes[node + 2].node, null);
            code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(code.items.len - offset));
        },
        .OR => { // if (!a) a else b
            assert(count == 2);
            effect += try compile(code, indices, tokens, nodes, nodes[node + 1].node, null);
            try code.append(opcode("dup"));
            try code.append(opcode("not"));
            try code.append(opcode("jif"));
            const offset = code.items.len;
            try code.appendNTimes(0xAA, N);
            try code.append(opcode("pop"));
            effect -= 1;
            effect += try compile(code, indices, tokens, nodes, nodes[node + 2].node, null);
            code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(code.items.len - offset));
        },
        .VAR => {
            // https://discord.com/channels/605571803288698900/1333444838691180554
            const map = &indices.first.?.data;
            const name = tokens[nodes[node + 1].node].src;
            const index: u24 = @truncate(map.count());
            try map.putNoClobber(name, index);
            if (count > 1) {
                effect += try compile(code, indices, tokens, nodes, nodes[node + 2].node, null);
                try code.append(opcode("set"));
                try code.appendSlice(&mem.toBytes(Index{ .depth = 0, .index = index }));
                effect -= 1;
            }
        },
        .EQUAL => {
            const name = tokens[nodes[nodes[node + 1].node].head.token].src;
            if (find(indices, name)) |index| {
                effect += try compile(code, indices, tokens, nodes, nodes[node + 2].node, null);
                try code.append(opcode("set"));
                try code.appendSlice(&mem.toBytes(index));
                effect -= 1;
            } else {
                std.debug.print("use of undeclared identifier '{s}'.", .{name});
                return error.UndeclaredIdentifier;
            }
        },
        .IDENTIFIER => {
            const name = token.src;
            if (find(indices, name)) |index| {
                try code.append(opcode("get"));
                try code.appendSlice(&mem.toBytes(index));
                effect += 1;
            } else {
                std.debug.print("use of undeclared identifier '{s}'.", .{name});
                return error.UndeclaredIdentifier;
            }
        },
        .IF => {
            assert(count == 2 or count == 3);
            effect += try compile(code, indices, tokens, nodes, nodes[node + 1].node, null);
            try code.append(opcode("jif"));
            const offset = code.items.len;
            try code.appendNTimes(0xAA, N);
            effect -= 1;
            effect += try compile(code, indices, tokens, nodes, nodes[node + 2].node, null);
            if (count == 3) {
                try code.append(opcode("jmp"));
                const other = code.items.len;
                try code.appendNTimes(0xAA, N);
                effect -= 1;
                code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(code.items.len - offset));
                effect += try compile(code, indices, tokens, nodes, nodes[node + 3].node, null);
                code.replaceRangeAssumeCapacity(other, N, &mem.toBytes(code.items.len - other));
            } else {
                code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(code.items.len - offset));
            }
        },
        .WHILE => {
            assert(count == 2);
            const start = code.items.len;
            effect += try compile(code, indices, tokens, nodes, nodes[node + 1].node, null);
            try code.append(opcode("jif"));
            const offset = code.items.len;
            try code.appendNTimes(0xAA, N);
            effect -= 1;
            effect += try compile(code, indices, tokens, nodes, nodes[node + 2].node, null);
            try code.append(opcode("ebb"));
            try code.appendSlice(&mem.toBytes(code.items.len - start));
            code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(code.items.len - offset));
        },
        .FOR => {
            assert(count == 4);
            effect += try compile(code, indices, tokens, nodes, nodes[node + 1].node, null);
            try code.appendNTimes(opcode("pop"), @abs(effect));
            effect -= effect;
            const start = code.items.len;
            effect += try compile(code, indices, tokens, nodes, nodes[node + 2].node, null);
            try code.append(opcode("jif"));
            const offset = code.items.len;
            try code.appendNTimes(0xAA, N);
            effect -= 1;
            effect += try compile(code, indices, tokens, nodes, nodes[node + 3].node, null);
            const n = try compile(code, indices, tokens, nodes, nodes[node + 4].node, null);
            try code.appendNTimes(opcode("pop"), @abs(n));
            effect -= n;
            try code.append(opcode("ebb"));
            try code.appendSlice(&mem.toBytes(code.items.len - start));
            code.replaceRangeAssumeCapacity(offset, N, &mem.toBytes(code.items.len - offset));
        },
        .RIGHT_BRACE => {
            var locals = Map.init(code.allocator);
            defer locals.deinit();
            try code.append(opcode("new"));
            const len = code.items.len;
            try code.append(0xAA); // make space for scope size
            if (params) |parms| {
                // function prologue (prime new scope with function parameters)
                try locals.ensureUnusedCapacity(@truncate(parms.len));
                for (parms, 0..) |param, i| {
                    locals.putAssumeCapacityNoClobber(tokens[param.head.token].src, @truncate(i));
                    try code.append(opcode("set"));
                    try code.appendSlice(&mem.toBytes(Index{ .depth = 0, .index = @truncate(i) }));
                    effect -= 1;
                }
            }
            var new = Indices.Node{ .data = locals };
            indices.prepend(&new);
            defer code.items[len] = @truncate(indices.popFirst().?.data.count());
            const index = node + 1;
            for (nodes[index .. index + count]) |child|
                effect += try compile(code, indices, tokens, nodes, child.node, params);
            try code.append(opcode("del"));
        },
        .FUN => {
            assert(count >= 2);
            // install function address in scope
            const map = &indices.first.?.data;
            const name = tokens[nodes[node + 1].node].src;
            const index: u24 = @truncate(map.count());
            try map.putNoClobber(name, index);
            try code.append(opcode("box"));
            const start = code.items.len;
            try code.appendNTimes(0xAA, N);
            try code.append(opcode("set"));
            try code.appendSlice(&mem.toBytes(Index{ .depth = 0, .index = index }));
            // skip over function body
            try code.append(opcode("jmp"));
            const body = code.items.len;
            try code.appendNTimes(0xAA, N);
            code.replaceRangeAssumeCapacity(start, N, &mem.toBytes(code.items.len));
            const parms = nodes[node + 2 .. node + count];
            const side_effect = try compile(code, indices, tokens, nodes, nodes[node + count].node, parms);
            assert(side_effect + @as(isize, @intCast(parms.len)) == 1);
            try code.append(opcode("ret"));
            code.replaceRangeAssumeCapacity(body, N, &mem.toBytes(code.items.len - body));
        },
        .RIGHT_PAREN => {
            for (0..count) |i|
                effect += try compile(code, indices, tokens, nodes, nodes[node + count - i].node, null);
            assert(effect == count);
            try code.append(opcode("call"));
            effect -= 1;
            try code.append(@truncate(count));
        },
        else => unreachable,
    }
    return effect;
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

    const n = try compile(&actual, &indices, tokens[0..], nodes[0..], 5, null);
    try testing.expectEqual(1, n);
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

    var code = std.ArrayList(u8).init(allocator);
    defer code.deinit();
    var globals = Indices.Node{ .data = Map.init(allocator) };
    defer globals.data.deinit();
    var indices = Indices{};
    indices.prepend(&globals);
    defer _ = indices.popFirst();
    const n = try compile(&code, &indices, tokens, nodes.items, root, null);

    var stack = try Stack.initCapacity(allocator, 16);
    defer stack.deinit();
    try run(code.items, &stack, values);

    std.debug.assert(stack.items.len == n);
    return stack.pop();
}

test execute {
    const allocator = testing.allocator;
    var values = Values{};
    var data: [2]value.Box = undefined;
    var globals = Values.Node{ .data = &data };
    values.prepend(&globals);

    try testing.expectEqual(2.0, try execute(allocator, "1 + 1", &values));
    try testing.expectEqual(1.0, try execute(allocator, "var a = 1; a", &values));
    try testing.expectEqual(0.0, try execute(allocator, "var a = 0; if (a) a = 2; a", &values));
    try testing.expectEqual(2.0, try execute(allocator, "var a = 1; if (a) a = 2; a", &values));
    try testing.expectEqual(0.0, try execute(allocator, "var a = 1; while (a) a = a - 1; a", &values));
    try testing.expectEqual(
        10.0,
        try execute(allocator, "var sum = 0; for (var i = 0; i < 5; i = i + 1) sum = sum + i; sum", &values),
    );
    try testing.expectEqual(
        1.0,
        try execute(allocator, "fun f(x) { x + 1 }; f(0)", &values),
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
