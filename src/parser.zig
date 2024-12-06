const std = @import("std");
const assert = std.debug.assert;
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Tokenizer = tokenizer.Tokenizer;

pub const Node = struct {
    token: Token,
    lhs: ?*Node,
    rhs: ?*Node,

    pub fn emit(self: @This(), src: []const u8, writer: anytype) !void {
        const slice = src[self.token.loc.start..self.token.loc.end];
        switch (self.token.tag) {
            .LEFT_PAREN => {
                try writer.print("(group ", .{});
                try self.lhs.?.emit(src, writer);
                try writer.print(")", .{});
            },
            .NUMBER => {
                const value = try std.fmt.parseFloat(f64, slice);
                var precision: ?usize = 1;
                if (std.mem.indexOfScalar(u8, slice, '.')) |index| {
                    if (std.mem.allEqual(u8, slice[index + 1 ..], '0') == false)
                        precision = null;
                }
                try writer.print("{d:.[1]}", .{ value, precision });
            },
            .STRING => try writer.print("{s}", .{slice[1 .. slice.len - 1]}),
            .FALSE, .NIL, .TRUE => try writer.print("{s}", .{slice}),
            else => {
                try writer.print("({s} ", .{slice});
                try self.lhs.?.emit(src, writer);
                if (self.rhs != null) {
                    try writer.print(" ", .{});
                    try self.rhs.?.emit(src, writer);
                }
                try writer.print(")", .{});
            },
        }
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: *Tokenizer,
    look_ahead: Token,

    pub fn init(allocator: std.mem.Allocator, tokens: *Tokenizer) Parser {
        return .{
            .allocator = allocator,
            .tokens = tokens,
            .look_ahead = tokens.next(),
        };
    }

    fn create(self: @This(), token: Token, lhs: ?*Node, rhs: ?*Node) error{OutOfMemory}!*Node {
        const node = try self.allocator.create(Node);
        node.* = .{ .token = token, .lhs = lhs, .rhs = rhs };
        return node;
    }

    fn peek(self: *Parser) Token {
        return self.look_ahead;
    }

    fn next(self: *Parser) Token {
        const token = self.peek();
        self.look_ahead = self.tokens.next();
        return token;
    }

    pub fn expression(self: *Parser) error{OutOfMemory}!*Node {
        return self.equality();
    }

    pub fn equality(self: *Parser) error{OutOfMemory}!*Node {
        var result = try self.comparison();
        while (true) {
            const token = self.peek();
            result = switch (token.tag) {
                .EQUAL_EQUAL, .BANG_EQUAL => try self.create(self.next(), result, try self.comparison()),
                else => break,
            };
        }
        return result;
    }

    pub fn comparison(self: *Parser) error{OutOfMemory}!*Node {
        var result = try self.term();
        while (true) {
            const token = self.peek();
            result = switch (token.tag) {
                .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => try self.create(self.next(), result, try self.term()),
                else => break,
            };
        }
        return result;
    }

    fn term(self: *Parser) error{OutOfMemory}!*Node {
        var result = try self.factor();
        while (true) {
            const token = self.peek();
            result = switch (token.tag) {
                .MINUS, .PLUS => try self.create(self.next(), result, try self.factor()),
                else => break,
            };
        }
        return result;
    }

    fn factor(self: *Parser) error{OutOfMemory}!*Node {
        var result = try self.unary();
        while (true) {
            const token = self.peek();
            result = switch (token.tag) {
                .STAR, .SLASH => try self.create(self.next(), result, try self.unary()),
                else => break,
            };
        }
        return result;
    }

    fn unary(self: *Parser) error{OutOfMemory}!*Node {
        const token = self.peek();
        return switch (token.tag) {
            .BANG, .MINUS => self.create(self.next(), try self.unary(), null),
            else => self.primary(),
        };
    }

    fn primary(self: *Parser) error{OutOfMemory}!*Node {
        const token = self.peek();
        return switch (token.tag) {
            .STRING, .NUMBER, .FALSE, .NIL, .TRUE => try self.create(self.next(), null, null),
            .LEFT_PAREN => blk: {
                const result = try self.create(self.next(), try self.expression(), null);
                assert(self.next().tag == .RIGHT_PAREN);
                break :blk result;
            },
            else => unreachable,
        };
    }
};
