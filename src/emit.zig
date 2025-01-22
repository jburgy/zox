const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const tokenize = @import("tokenize.zig");
const parse = @import("parse.zig");

pub fn emit(tokens: []const tokenize.Token, nodes: []const parse.Node, node: usize, writer: anytype) !void {
    const token = tokens[nodes[node].head.token];
    const slice = switch (token.tag) {
        .EOF => "prog",
        .LEFT_PAREN => "group",
        .RIGHT_BRACE => "block",
        .RIGHT_PAREN => "apply",
        .SEMICOLON => "",
        else => token.src,
    };
    const delimiter = switch (token.tag) {
        .RIGHT_BRACE, .EOF => "\n",
        else => " ",
    };
    switch (token.tag) {
        .NUMBER => {
            const value = try std.fmt.parseFloat(f64, slice);
            var precision: ?usize = 1;
            if (std.mem.indexOfScalar(u8, slice, '.')) |index| {
                if (std.mem.allEqual(u8, slice[index + 1 ..], '0') == false)
                    precision = null;
            }
            try writer.print("{d:.[1]}", .{ value, precision });
        },
        .FALSE, .NIL, .TRUE, .IDENTIFIER, .STRING => try writer.print("{s}", .{slice}),
        .VAR => {
            try writer.print("({s}{s}{s}", .{ slice, delimiter, tokens[nodes[node + 1].head.token].src });
            if (nodes[node].head.count == 2) {
                try writer.print("{s}", .{delimiter});
                try emit(tokens, nodes, nodes[node + 2].node, writer);
            }
            try writer.print(")", .{});
        },
        .FUN => {
            const index = node + 1;
            const count = nodes[node].head.count - 1;
            try writer.print("({s}", .{slice});
            for (nodes[index .. index + count]) |arg|
                try writer.print("{s}{s}", .{ delimiter, tokens[arg.head.token].src });
            try writer.print("{s}", .{delimiter});
            try emit(tokens, nodes, nodes[index + count].node, writer);
            try writer.print(")", .{});
        },
        else => {
            const index = node + 1;
            const count = nodes[node].head.count;
            try writer.print("({s}", .{slice});
            for (nodes[index .. index + count]) |arg| {
                try writer.print("{s}", .{delimiter});
                try emit(tokens, nodes, arg.node, writer);
            }
            try writer.print(")", .{});
        },
    }
}

test emit {
    const allocator = testing.allocator;
    const buffer =
        \\fun makeAccumulator(label) {
        \\  var sum = 0;
        \\  var count = 0;
        \\
        \\  fun accumulate(value) {
        \\    sum = sum + value;
        \\    count = count + 1;
        \\
        \\    print label;
        \\    print count;
        \\    print sum;
        \\    print sum;
        \\
        \\    if (count > 3) {
        \\      print "reset";
        \\      sum = 0;
        \\      count = 0;
        \\    }
        \\
        \\     return sum;
        \\  }
        \\
        \\  return accumulate;
        \\}
        \\
        \\var acc1 = makeAccumulator("First:");
        \\var acc2 = makeAccumulator("Second:");
        \\
        \\acc1(2);
        \\acc1(6);
        \\acc1(3);
        \\acc1(4);
        \\
        \\acc2(6);
        \\acc2(4);
    ;

    const tokens = try tokenize.tokens(allocator, buffer);
    defer allocator.free(tokens);

    var nodes = parse.Nodes.init(allocator);
    defer nodes.deinit();

    const state = try parse.statements(&nodes, tokens, 0);
    std.debug.assert(tokens[state.token].tag == .EOF);
    const root = state.node.node;
    std.debug.assert(root + 1 + nodes.items[root].head.count == nodes.items.len);

    var actual = try std.ArrayListUnmanaged(u8).initCapacity(allocator, 512);
    defer actual.deinit(allocator);

    try emit(tokens, nodes.items, root, actual.writer(allocator));

    const expected =
        \\(prog
        \\(fun makeAccumulator label (block
        \\(var sum 0.0)
        \\(var count 0.0)
        \\(fun accumulate value (block
        \\(= sum (+ sum value))
        \\(= count (+ count 1.0))
        \\(print label)
        \\(print count)
        \\(print sum)
        \\(print sum)
        \\(if (> count 3.0) (block
        \\(print "reset")
        \\(= sum 0.0)
        \\(= count 0.0)))
        \\(return sum)))
        \\(return accumulate)))
        \\(var acc1 (apply makeAccumulator "First:"))
        \\(var acc2 (apply makeAccumulator "Second:"))
        \\(apply acc1 2.0)
        \\(apply acc1 6.0)
        \\(apply acc1 3.0)
        \\(apply acc1 4.0)
        \\(apply acc2 6.0)
        \\(apply acc2 4.0))
    ;

    try testing.expectEqualStrings(expected, actual.items);
}
