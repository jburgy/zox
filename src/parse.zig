const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;
const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;

const ParseError = error{
    OutOfMemory,
    UnexpectedToken,
};

pub const Node = packed union {
    head: packed struct(u32) { token: u24, count: u8 },
    node: u32,
};
const State = struct { token: u24, node: Node };
const Nodes = std.ArrayListUnmanaged(Node);

fn head(token: u24, count: u8) Node {
    return .{ .head = .{ .token = token, .count = count } };
}

fn ref(node: u32) Node {
    return .{ .node = node };
}

fn appendNode(nodes: *Nodes, allocator: Allocator, token: u24, args: []const Node) Allocator.Error!Node {
    const node = Node{ .node = @truncate(nodes.items.len) };
    try nodes.append(allocator, head(token, @truncate(args.len)));
    try nodes.appendSlice(allocator, args);
    return node;
}

pub fn statements(nodes: *Nodes, allocator: Allocator, tokens: []const Token, start: u24) ParseError!State {
    var token = start;
    var args = try Nodes.initCapacity(allocator, 4);
    defer args.deinit(allocator);

    while (true) {
        switch (tokens[token].tag) {
            .RIGHT_BRACE, .EOF => break,
            else => {
                const state = try statement(nodes, allocator, tokens, token);
                try args.append(allocator, state.node);
                token = state.token + @intFromBool(tokens[state.token].tag == .SEMICOLON);
            },
        }
    }
    return .{
        .token = token,
        .node = try appendNode(nodes, allocator, token, args.items),
    };
}

fn statement(nodes: *Nodes, allocator: Allocator, tokens: []const Token, token: u24) ParseError!State {
    return switch (tokens[token].tag) {
        .PRINT => blk: {
            const next = try expression(nodes, allocator, tokens, token + 1);
            break :blk .{
                .token = next.token,
                .node = try appendNode(nodes, allocator, token, &.{next.node}),
            };
        },
        .VAR => blk: {
            if (tokens[token + 1].tag != .IDENTIFIER)
                break :blk error.UnexpectedToken;

            if (tokens[token + 2].tag == .SEMICOLON)
                break :blk .{
                    .token = token + 2,
                    .node = try appendNode(nodes, allocator, token, &.{head(token + 1, 0)}),
                };

            const next = try expression(nodes, allocator, tokens, token + 3);
            break :blk .{
                .token = next.token,
                .node = try appendNode(nodes, allocator, token, &.{ head(token + 1, 0), next.node }),
            };
        },
        .LEFT_BRACE => blk: {
            const next = try statements(nodes, allocator, tokens, token + 1);
            break :blk if (tokens[next.token].tag != .RIGHT_BRACE)
                error.UnexpectedToken
            else
                .{ .token = next.token + 1, .node = next.node };
        },
        .IF => blk: {
            if (tokens[token + 1].tag != .LEFT_PAREN)
                break :blk error.UnexpectedToken;
            const cond = try expression(nodes, allocator, tokens, token + 2);
            if (tokens[cond.token].tag != .RIGHT_PAREN)
                break :blk error.UnexpectedToken;

            const body = try statement(nodes, allocator, tokens, cond.token + 1);
            if (tokens[body.token].tag == .ELSE) {
                const tail = try statement(nodes, allocator, tokens, body.token + 1);
                break :blk .{
                    .token = tail.token,
                    .node = try appendNode(nodes, allocator, token, &.{ cond.node, body.node, tail.node }),
                };
            } else {
                break :blk .{
                    .token = body.token,
                    .node = try appendNode(nodes, allocator, token, &.{ cond.node, body.node }),
                };
            }
        },
        .WHILE => blk: {
            switch (tokens[token + 1].tag) {
                .LEFT_PAREN => {
                    const cond = try expression(nodes, allocator, tokens, token + 2);
                    switch (tokens[cond.token].tag) {
                        .RIGHT_PAREN => {
                            const body = try statement(nodes, allocator, tokens, cond.token + 1);
                            break :blk .{
                                .token = body.token,
                                .node = try appendNode(nodes, allocator, token, &.{body.node}),
                            };
                        },
                        else => break :blk error.UnexpectedToken,
                    }
                },
                else => break :blk error.UnexpectedToken,
            }
        },
        .FOR => blk: {
            if (tokens[token + 1].tag != .LEFT_PAREN)
                break :blk error.UnexpectedToken;

            const init: State = switch (tokens[token + 2].tag) {
                .SEMICOLON => .{
                    .token = token + 2,
                    .node = try appendNode(nodes, allocator, token + 2, &.{}),
                },
                .LEFT_BRACE => break :blk error.UnexpectedToken,
                else => try statement(nodes, allocator, tokens, token + 2),
            };
            if (tokens[init.token].tag != .SEMICOLON)
                break :blk error.UnexpectedToken;

            const cond = try expression(nodes, allocator, tokens, init.token + 1);
            if (tokens[cond.token].tag != .SEMICOLON)
                break :blk error.UnexpectedToken;

            const incr = switch (tokens[cond.token + 1].tag) {
                .RIGHT_PAREN => State{
                    .token = cond.token + 1,
                    .node = try appendNode(nodes, allocator, cond.token, &.{}),
                },
                else => try expression(nodes, allocator, tokens, cond.token + 1),
            };
            if (tokens[incr.token].tag != .RIGHT_PAREN)
                break :blk error.UnexpectedToken;
            const body = try statement(nodes, allocator, tokens, incr.token + 1);
            break :blk .{
                .token = body.token,
                .node = try appendNode(nodes, allocator, token, &.{ init.node, cond.node, body.node, incr.node }),
            };
        },
        .FUN => blk: {
            var index = token + 1;
            var args = try Nodes.initCapacity(allocator, 2);
            defer args.deinit(allocator);

            if (tokens[index].tag != .IDENTIFIER)
                break :blk error.UnexpectedToken;

            try args.append(allocator, head(index, 0));
            if (tokens[index + 1].tag != .LEFT_PAREN)
                break :blk error.UnexpectedToken;
            index += 2; // skip name and opening paren

            while (true) {
                switch (tokens[index].tag) {
                    .RIGHT_PAREN => break,
                    .IDENTIFIER => {
                        try args.append(allocator, head(index, 0));
                        index += 1;
                        switch (tokens[index].tag) {
                            .COMMA => index += 1,
                            .RIGHT_PAREN => {},
                            else => break :blk error.UnexpectedToken,
                        }
                    },
                    else => break :blk error.UnexpectedToken,
                }
            }
            if (tokens[index + 1].tag != .LEFT_BRACE)
                break :blk error.UnexpectedToken;

            const body = try statement(nodes, allocator, tokens, index + 1);
            try args.append(allocator, body.node);

            break :blk .{
                .token = body.token,
                .node = try appendNode(nodes, allocator, token, args.items),
            };
        },
        .RETURN => blk: {
            if (tokens[token + 1].tag == .SEMICOLON)
                break :blk .{ .token = token + 1, .node = try appendNode(nodes, allocator, token, &.{}) };

            const result = try expression(nodes, allocator, tokens, token + 1);
            break :blk .{
                .token = result.token,
                .node = try appendNode(nodes, allocator, token, &.{result.node}),
            };
        },
        else => try expression(nodes, allocator, tokens, token),
    };
}

fn expression(nodes: *Nodes, allocator: Allocator, tokens: []const Token, index: u24) ParseError!State {
    var result = try equality(nodes, allocator, tokens, index);
    while (true) {
        switch (tokens[result.token].tag) {
            .EQUAL, .OR, .AND => {
                const next = try expression(nodes, allocator, tokens, result.token + 1);
                const node = try appendNode(nodes, allocator, result.token, &.{ result.node, next.node });
                result = .{ .token = next.token, .node = node };
            },
            else => break,
        }
    }
    return result;
}

fn equality(nodes: *Nodes, allocator: Allocator, tokens: []const Token, index: u24) ParseError!State {
    var result = try comparison(nodes, allocator, tokens, index);
    while (true) {
        switch (tokens[result.token].tag) {
            .EQUAL_EQUAL, .BANG_EQUAL => {
                const next = try comparison(nodes, allocator, tokens, result.token + 1);
                const node = try appendNode(nodes, allocator, result.token, &.{ result.node, next.node });
                result = .{ .token = next.token, .node = node };
            },
            else => break,
        }
    }
    return result;
}

fn comparison(nodes: *Nodes, allocator: Allocator, tokens: []const Token, index: u24) ParseError!State {
    var result = try term(nodes, allocator, tokens, index);
    while (true) {
        switch (tokens[result.token].tag) {
            .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => {
                const next = try term(nodes, allocator, tokens, result.token + 1);
                const node = try appendNode(nodes, allocator, result.token, &.{ result.node, next.node });
                result = .{ .token = next.token, .node = node };
            },
            else => break,
        }
    }
    return result;
}

fn term(nodes: *Nodes, allocator: Allocator, tokens: []const Token, index: u24) ParseError!State {
    var result = try factor(nodes, allocator, tokens, index);
    while (true) {
        switch (tokens[result.token].tag) {
            .MINUS, .PLUS => {
                const next = try factor(nodes, allocator, tokens, result.token + 1);
                const node = try appendNode(nodes, allocator, result.token, &.{ result.node, next.node });
                result = .{ .token = next.token, .node = node };
            },
            else => break,
        }
    }
    return result;
}

fn factor(nodes: *Nodes, allocator: Allocator, tokens: []const Token, token: u24) ParseError!State {
    var result = try unary(nodes, allocator, tokens, token);
    while (true) {
        switch (tokens[result.token].tag) {
            .STAR, .SLASH => {
                const next = try unary(nodes, allocator, tokens, result.token + 1);
                const node = try appendNode(nodes, allocator, result.token, &.{ result.node, next.node });
                result = .{ .token = next.token, .node = node };
            },
            else => break,
        }
    }
    return result;
}

fn unary(nodes: *Nodes, allocator: Allocator, tokens: []const Token, token: u24) ParseError!State {
    return switch (tokens[token].tag) {
        .BANG, .MINUS => blk: {
            const next = try unary(nodes, allocator, tokens, token + 1);
            const node = try appendNode(nodes, allocator, token, &.{next.node});
            break :blk .{ .token = next.token, .node = node };
        },
        else => try call(nodes, allocator, tokens, token),
    };
}

fn call(nodes: *Nodes, allocator: Allocator, tokens: []const Token, token: u24) ParseError!State {
    var result = try primary(nodes, allocator, tokens, token);
    while (tokens[result.token].tag == .LEFT_PAREN)
        result = try finishCall(nodes, allocator, tokens, result);
    return result;
}

fn finishCall(nodes: *Nodes, allocator: Allocator, tokens: []const Token, func: State) ParseError!State {
    var index = func.token + 1;
    var args = try Nodes.initCapacity(allocator, 4);
    defer args.deinit(allocator);

    try args.append(allocator, func.node);
    while (tokens[index].tag != .RIGHT_PAREN) {
        const state = try expression(nodes, allocator, tokens, index);
        try args.append(allocator, state.node);
        index = state.token + @intFromBool(tokens[state.token].tag == .COMMA);
    }
    return .{
        .token = index + 1,
        .node = try appendNode(nodes, allocator, index, args.items),
    };
}

fn primary(nodes: *Nodes, allocator: Allocator, tokens: []const Token, token: u24) ParseError!State {
    return switch (tokens[token].tag) {
        .NIL, .FALSE, .TRUE, .NUMBER, .STRING, .IDENTIFIER => .{
            .token = token + 1,
            .node = try appendNode(nodes, allocator, token, &.{}),
        },
        .LEFT_PAREN => blk: {
            const state = try expression(nodes, allocator, tokens, token + 1);
            break :blk switch (tokens[state.token].tag) {
                .RIGHT_PAREN => .{ .token = state.token + 1, .node = state.node },
                else => error.UnexpectedToken,
            };
        },
        else => error.UnexpectedToken,
    };
}

fn helper(allocator: Allocator, buffer: []const u8) ![]const Node {
    const tokens = try tokenize.tokens(allocator, buffer);
    defer allocator.free(tokens);

    var nodes = try Nodes.initCapacity(allocator, 16);
    defer nodes.deinit(allocator);

    const state = try statements(&nodes, allocator, tokens, 0);
    std.debug.assert(tokens[state.token].tag == .EOF);
    const root = state.node.node;
    std.debug.assert(root + 1 + nodes.items[root].head.count == nodes.items.len);

    return nodes.toOwnedSlice(allocator);
}

test statements {
    const allocator = testing.allocator;

    const cases = [_]struct { buffer: []const u8, expected: []const Node }{
        .{ .buffer = "true", .expected = &.{ head(0, 0), head(1, 0) } },
        .{ .buffer = "a = b", .expected = &.{
            head(0, 0),
            head(2, 0),
            head(0, 2),
            ref(3),
            ref(1),
            ref(4),
        } },
        // .{ .buffer = "a + b", .expected = &.{ 0, 0, 2, 0, 1, 2, 0, 2, 3, 1, 4 } },
        // .{ .buffer = "a + b * c", .expected = &.{ 0, 0, 2, 0, 4, 0, 3, 2, 2, 4, 1, 2, 0, 6, 5, 1, 10 } },
        // .{ .buffer = "var a;", .expected = &.{ 0, 1, 1, 3, 1, 0 } },
        // .{ .buffer = "var a = 0;", .expected = &.{ 3, 0, 0, 2, 1, 0, 5, 1, 2 } },
        // .{ .buffer = "var a; a = 1", .expected = &.{ 0, 1, 1, 3, 0, 5, 0, 4, 2, 3, 5, 6, 2, 0, 7 } },
        // .{ .buffer = "var a = 0; a = 1", .expected = &.{ 3, 0, 0, 2, 1, 0, 5, 0, 7, 0, 6, 2, 6, 8, 8, 2, 2, 10 } },
    };
    for (cases) |case| {
        const actual = try helper(allocator, case.buffer);
        defer allocator.free(actual);
        try testing.expect(std.mem.eql(u8, mem.asBytes(case.expected), mem.asBytes(actual)));
    }
}
