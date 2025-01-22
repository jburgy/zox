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
pub const Nodes = std.ArrayList(Node);
const Args = std.ArrayListUnmanaged(Node);
const MaxArgs = std.math.maxInt(std.meta.FieldType(std.meta.FieldType(Node, .head), .count));

fn head(token: u24, count: u8) Node {
    return .{ .head = .{ .token = token, .count = count } };
}

fn ref(node: u32) Node {
    return .{ .node = node };
}

fn appendNode(nodes: *Nodes, token: u24, args: []const Node) Allocator.Error!Node {
    const node = Node{ .node = @truncate(nodes.items.len) };
    try nodes.append(head(token, @truncate(args.len)));
    try nodes.appendSlice(args);
    return node;
}

pub fn statements(nodes: *Nodes, tokens: []const Token, start: u24) ParseError!State {
    var token = start;
    var buffer: [MaxArgs]Node = undefined;
    var args = Args.initBuffer(buffer[0..]);

    while (true) {
        switch (tokens[token].tag) {
            .RIGHT_BRACE, .EOF => break,
            else => {
                const state = try statement(nodes, tokens, token);
                args.appendAssumeCapacity(state.node);
                token = state.token + @intFromBool(tokens[state.token].tag == .SEMICOLON);
            },
        }
    }
    return .{
        .token = token,
        .node = try appendNode(nodes, token, args.items),
    };
}

fn statement(nodes: *Nodes, tokens: []const Token, token: u24) ParseError!State {
    return switch (tokens[token].tag) {
        .PRINT => blk: {
            const next = try expression(nodes, tokens, token + 1);
            break :blk .{
                .token = next.token,
                .node = try appendNode(nodes, token, &.{next.node}),
            };
        },
        .VAR => blk: {
            if (tokens[token + 1].tag != .IDENTIFIER)
                break :blk error.UnexpectedToken;

            if (tokens[token + 2].tag == .SEMICOLON)
                break :blk .{
                    .token = token + 2,
                    .node = try appendNode(nodes, token, &.{head(token + 1, 0)}),
                };

            const next = try expression(nodes, tokens, token + 3);
            break :blk .{
                .token = next.token,
                .node = try appendNode(nodes, token, &.{ head(token + 1, 0), next.node }),
            };
        },
        .LEFT_BRACE => blk: {
            const next = try statements(nodes, tokens, token + 1);
            break :blk if (tokens[next.token].tag != .RIGHT_BRACE)
                error.UnexpectedToken
            else
                .{ .token = next.token + 1, .node = next.node };
        },
        .IF => blk: {
            if (tokens[token + 1].tag != .LEFT_PAREN)
                break :blk error.UnexpectedToken;
            const cond = try expression(nodes, tokens, token + 2);
            if (tokens[cond.token].tag != .RIGHT_PAREN)
                break :blk error.UnexpectedToken;

            const body = try statement(nodes, tokens, cond.token + 1);
            if (tokens[body.token].tag == .ELSE) {
                const tail = try statement(nodes, tokens, body.token + 1);
                break :blk .{
                    .token = tail.token,
                    .node = try appendNode(nodes, token, &.{ cond.node, body.node, tail.node }),
                };
            } else {
                break :blk .{
                    .token = body.token,
                    .node = try appendNode(nodes, token, &.{ cond.node, body.node }),
                };
            }
        },
        .WHILE => blk: {
            switch (tokens[token + 1].tag) {
                .LEFT_PAREN => {
                    const cond = try expression(nodes, tokens, token + 2);
                    switch (tokens[cond.token].tag) {
                        .RIGHT_PAREN => {
                            const body = try statement(nodes, tokens, cond.token + 1);
                            break :blk .{
                                .token = body.token,
                                .node = try appendNode(nodes, token, &.{body.node}),
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
                    .node = try appendNode(nodes, token + 2, &.{}),
                },
                .LEFT_BRACE => break :blk error.UnexpectedToken,
                else => try statement(nodes, tokens, token + 2),
            };
            if (tokens[init.token].tag != .SEMICOLON)
                break :blk error.UnexpectedToken;

            const cond = try expression(nodes, tokens, init.token + 1);
            if (tokens[cond.token].tag != .SEMICOLON)
                break :blk error.UnexpectedToken;

            const incr = switch (tokens[cond.token + 1].tag) {
                .RIGHT_PAREN => State{
                    .token = cond.token + 1,
                    .node = try appendNode(nodes, cond.token, &.{}),
                },
                else => try expression(nodes, tokens, cond.token + 1),
            };
            if (tokens[incr.token].tag != .RIGHT_PAREN)
                break :blk error.UnexpectedToken;
            const body = try statement(nodes, tokens, incr.token + 1);
            break :blk .{
                .token = body.token,
                .node = try appendNode(nodes, token, &.{ init.node, cond.node, body.node, incr.node }),
            };
        },
        .FUN => blk: {
            var index = token + 1;
            var buffer: [MaxArgs]Node = undefined;
            var args = Args.initBuffer(buffer[0..]);

            if (tokens[index].tag != .IDENTIFIER)
                break :blk error.UnexpectedToken;

            args.appendAssumeCapacity(head(index, 0));
            if (tokens[index + 1].tag != .LEFT_PAREN)
                break :blk error.UnexpectedToken;
            index += 2; // skip name and opening paren

            while (true) {
                switch (tokens[index].tag) {
                    .RIGHT_PAREN => break,
                    .IDENTIFIER => {
                        args.appendAssumeCapacity(head(index, 0));
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

            const body = try statement(nodes, tokens, index + 1);
            args.appendAssumeCapacity(body.node);

            break :blk .{
                .token = body.token,
                .node = try appendNode(nodes, token, args.items),
            };
        },
        .RETURN => blk: {
            if (tokens[token + 1].tag == .SEMICOLON)
                break :blk .{ .token = token + 1, .node = try appendNode(nodes, token, &.{}) };

            const result = try expression(nodes, tokens, token + 1);
            break :blk .{
                .token = result.token,
                .node = try appendNode(nodes, token, &.{result.node}),
            };
        },
        else => try expression(nodes, tokens, token),
    };
}

fn expression(nodes: *Nodes, tokens: []const Token, index: u24) ParseError!State {
    var result = try equality(nodes, tokens, index);
    while (true) {
        switch (tokens[result.token].tag) {
            .EQUAL, .OR, .AND => {
                const next = try equality(nodes, tokens, result.token + 1);
                result.node = try appendNode(nodes, result.token, &.{ result.node, next.node });
                result.token = next.token;
            },
            else => break,
        }
    }
    return result;
}

fn equality(nodes: *Nodes, tokens: []const Token, index: u24) ParseError!State {
    var result = try comparison(nodes, tokens, index);
    while (true) {
        switch (tokens[result.token].tag) {
            .EQUAL_EQUAL, .BANG_EQUAL => {
                const next = try comparison(nodes, tokens, result.token + 1);
                result.node = try appendNode(nodes, result.token, &.{ result.node, next.node });
                result.token = next.token;
            },
            else => break,
        }
    }
    return result;
}

fn comparison(nodes: *Nodes, tokens: []const Token, index: u24) ParseError!State {
    var result = try term(nodes, tokens, index);
    while (true) {
        switch (tokens[result.token].tag) {
            .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => {
                const next = try term(nodes, tokens, result.token + 1);
                result.node = try appendNode(nodes, result.token, &.{ result.node, next.node });
                result.token = next.token;
            },
            else => break,
        }
    }
    return result;
}

fn term(nodes: *Nodes, tokens: []const Token, index: u24) ParseError!State {
    var result = try factor(nodes, tokens, index);
    while (true) {
        switch (tokens[result.token].tag) {
            .MINUS, .PLUS => {
                const next = try factor(nodes, tokens, result.token + 1);
                result.node = try appendNode(nodes, result.token, &.{ result.node, next.node });
                result.token = next.token;
            },
            else => break,
        }
    }
    return result;
}

fn factor(nodes: *Nodes, tokens: []const Token, token: u24) ParseError!State {
    var result = try unary(nodes, tokens, token);
    while (true) {
        switch (tokens[result.token].tag) {
            .STAR, .SLASH => {
                const next = try unary(nodes, tokens, result.token + 1);
                result.node = try appendNode(nodes, result.token, &.{ result.node, next.node });
                result.token = next.token;
            },
            else => break,
        }
    }
    return result;
}

fn unary(nodes: *Nodes, tokens: []const Token, token: u24) ParseError!State {
    return switch (tokens[token].tag) {
        .BANG, .MINUS => blk: {
            const next = try unary(nodes, tokens, token + 1);
            const node = try appendNode(nodes, token, &.{next.node});
            break :blk .{ .token = next.token, .node = node };
        },
        else => try call(nodes, tokens, token),
    };
}

fn call(nodes: *Nodes, tokens: []const Token, token: u24) ParseError!State {
    var result = try primary(nodes, tokens, token);
    while (tokens[result.token].tag == .LEFT_PAREN)
        result = try finishCall(nodes, tokens, result);
    return result;
}

fn finishCall(nodes: *Nodes, tokens: []const Token, func: State) ParseError!State {
    var index = func.token + 1;
    var buffer: [MaxArgs]Node = undefined;
    var args = Args.initBuffer(buffer[0..]);

    args.appendAssumeCapacity(func.node);
    while (tokens[index].tag != .RIGHT_PAREN) {
        const state = try expression(nodes, tokens, index);
        args.appendAssumeCapacity(state.node);
        index = state.token + @intFromBool(tokens[state.token].tag == .COMMA);
    }
    return .{
        .token = index + 1,
        .node = try appendNode(nodes, index, args.items),
    };
}

fn primary(nodes: *Nodes, tokens: []const Token, token: u24) ParseError!State {
    return switch (tokens[token].tag) {
        .NIL, .FALSE, .TRUE, .NUMBER, .STRING, .IDENTIFIER => .{
            .token = token + 1,
            .node = try appendNode(nodes, token, &.{}),
        },
        .LEFT_PAREN => blk: {
            const state = try expression(nodes, tokens, token + 1);
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

    var nodes = Nodes.init(allocator);
    defer nodes.deinit();

    const state = try statements(&nodes, tokens, 0);
    std.debug.assert(tokens[state.token].tag == .EOF);
    const root = state.node.node;
    std.debug.assert(root + 1 + nodes.items[root].head.count == nodes.items.len);

    return nodes.toOwnedSlice();
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
        .{ .buffer = "a + b", .expected = &.{
            head(0, 0),
            head(2, 0),
            head(1, 2),
            ref(0),
            ref(2),
            head(3, 1),
            ref(4),
        } },
        .{ .buffer = "a + b * c", .expected = &.{
            head(0, 0),
            head(2, 0),
            head(4, 0),
            head(3, 2),
            ref(2),
            ref(4),
            head(2, 2),
            ref(0),
            ref(6),
            head(5, 1),
            ref(10),
        } },
        .{ .buffer = "var a;", .expected = &.{
            head(0, 1),
            ref(1),
            head(3, 1),
            ref(0),
        } },
        .{ .buffer = "var a = 0;", .expected = &.{
            head(3, 0),
            head(0, 2),
            ref(1),
            ref(0),
            head(5, 1),
            ref(2),
        } },
        .{ .buffer = "var a; a = 1", .expected = &.{
            head(0, 1),
            ref(1),
            head(3, 0),
            head(5, 0),
            head(4, 2),
            ref(3),
            ref(5),
            head(6, 2),
            ref(0),
            ref(7),
        } },
        .{ .buffer = "var a = 0; a = 1", .expected = &.{
            head(3, 0),
            head(0, 2),
            ref(1),
            ref(0),
            head(5, 0),
            head(7, 0),
            head(6, 2),
            ref(6),
            ref(8),
            head(8, 2),
            ref(2),
            ref(10),
        } },
    };
    for (cases) |case| {
        const actual = try helper(allocator, case.buffer);
        defer allocator.free(actual);
        try testing.expectEqualStrings(mem.asBytes(case.expected), mem.asBytes(actual));
    }
}
