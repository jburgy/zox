const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;
const Token = @import("tokenize.zig").Token;
const Node = @import("parse.zig").Node;
const opcode = @import("execute.zig").opcode;

// See https://github.com/ziglang/zig/pull/20074
const greedy = init: {
    var temp = std.EnumMap(Token.Tag, u8){};
    temp.put(.BANG_EQUAL, opcode("neq"));
    temp.put(.EQUAL_EQUAL, opcode("eql"));
    temp.put(.MINUS, opcode("sub"));
    temp.put(.PLUS, opcode("add"));
    temp.put(.SLASH, opcode("div"));
    temp.put(.STAR, opcode("mul"));
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
        },
        .NUMBER => {
            const num = try std.fmt.parseFloat(f64, token.src);
            const buf: [@sizeOf(f64)]u8 = @bitCast(num);
            try program.append(opcode("num"));
            try program.appendSlice(buf[0..]);
        },
        .BANG_EQUAL, .EQUAL_EQUAL, .MINUS, .PLUS, .SLASH, .STAR => {
            if (count == 1) { // -x ⇒ 0 - x
                try program.append(opcode("num"));
                try program.appendSlice(&[_]u8{0x00} ** @sizeOf(f64));
                try compile(program, tokens, nodes, nodes[node + 1].node);
            } else {
                try compile(program, tokens, nodes, nodes[node + 1].node);
                try compile(program, tokens, nodes, nodes[node + 2].node);
            }
            try program.append(greedy.get(token.tag).?);
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
    const one = [_]u8{0x00} ** (@sizeOf(f64) - 2) ++ .{ 0xf0, 0x3f };
    const expected = .{opcode("num")} ++ one ++ .{opcode("num")} ++ one ++ .{opcode("add")};

    try compile(&actual, tokens[0..], nodes[0..], 5);
    try testing.expectEqualDeep(expected[0..], actual.items);
}
