const std = @import("std");
const assert = std.debug.assert;
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Tokenizer = tokenizer.Tokenizer;

const stdout = std.io.getStdOut().writer();

const ParseError = error{
    OutOfMemory,
    UnexpectedToken,
    MissingSemicolon,
};

const EvaluationError = error{
    OperandMustBeANumber,
    OperandsMustBeNumbers,
};

const ValueType = enum {
    nil,
    bool,
    string,
    number,
};

const Value = union(ValueType) {
    nil: void,
    bool: bool,
    string: []const u8,
    number: f64,

    pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try switch (value) {
            .nil => writer.print("nil", .{}),
            .bool => writer.print("{any}", .{value.bool}),
            .string => writer.print("{s}", .{value.string}),
            .number => writer.print("{d}", .{value.number}),
            // else => unreachable,
        };
    }
};

pub const Node = struct {
    token: Token,
    args: []const *Node,

    pub fn emit(self: @This(), src: []const u8, writer: anytype) !void {
        const slice = src[self.token.loc.start..self.token.loc.end];
        switch (self.token.tag) {
            .LEFT_PAREN => {
                try writer.print("(group ", .{});
                try self.args[0].emit(src, writer);
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

                try self.args[0].emit(src, writer);
                for (self.args[1..]) |arg| {
                    try writer.print(" ", .{});
                    try arg.emit(src, writer);
                }
                try writer.print(")", .{});
            },
        }
    }

    pub fn evaluate(self: @This(), src: []const u8, allocator: std.mem.Allocator) !Value {
        const slice = src[self.token.loc.start..self.token.loc.end];
        return switch (self.token.tag) {
            .FALSE => .{ .bool = false },
            .NIL => .{ .nil = void{} },
            .TRUE => .{ .bool = true },
            .STRING => if (slice[slice.len - 1] == '"') .{ .string = slice[1 .. slice.len - 1] } else error.UnexpectedToken,
            .NUMBER => .{ .number = try std.fmt.parseFloat(f64, slice) },
            .LEFT_PAREN => try self.args[0].evaluate(src, allocator),
            .MINUS => if (self.args.len == 1) switch (try self.args[0].evaluate(src, allocator)) {
                .number => |lhs| .{ .number = -lhs },
                else => error.OperandMustBeANumber,
            } else switch (try self.args[0].evaluate(src, allocator)) {
                .number => |lhs| switch (try self.args[1].evaluate(src, allocator)) {
                    .number => |rhs| .{ .number = lhs - rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .PLUS => switch (try self.args[0].evaluate(src, allocator)) {
                .number => |lhs| switch (try self.args[1].evaluate(src, allocator)) {
                    .number => |rhs| .{ .number = lhs + rhs },
                    else => error.OperandsMustBeNumbers,
                },
                .string => |lhs| switch (try self.args[1].evaluate(src, allocator)) {
                    .string => |rhs| blk: {
                        const str = try allocator.alloc(u8, lhs.len + rhs.len);
                        @memcpy(str[0..lhs.len], lhs);
                        @memcpy(str[lhs.len..], rhs);
                        break :blk .{ .string = str };
                    },
                    else => error.OperandsMustBeNumbers,
                },
                else => unreachable,
            },
            .BANG => .{ .bool = switch (try self.args[0].evaluate(src, allocator)) {
                .nil => true,
                .bool => |b| b == false,
                .string => |s| std.mem.eql(u8, s, "") == false,
                .number => |x| x == 0.0,
            } },
            .STAR => switch (try self.args[0].evaluate(src, allocator)) {
                .number => |lhs| switch (try self.args[1].evaluate(src, allocator)) {
                    .number => |rhs| .{ .number = lhs * rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .SLASH => switch (try self.args[0].evaluate(src, allocator)) {
                .number => |lhs| switch (try self.args[1].evaluate(src, allocator)) {
                    .number => |rhs| .{ .number = lhs / rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .LESS => switch (try self.args[0].evaluate(src, allocator)) {
                .number => |lhs| switch (try self.args[1].evaluate(src, allocator)) {
                    .number => |rhs| .{ .bool = lhs < rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .LESS_EQUAL => switch (try self.args[0].evaluate(src, allocator)) {
                .number => |lhs| switch (try self.args[1].evaluate(src, allocator)) {
                    .number => |rhs| .{ .bool = lhs <= rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .GREATER => switch (try self.args[0].evaluate(src, allocator)) {
                .number => |lhs| switch (try self.args[1].evaluate(src, allocator)) {
                    .number => |rhs| .{ .bool = lhs > rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .GREATER_EQUAL => switch (try self.args[0].evaluate(src, allocator)) {
                .number => |lhs| switch (try self.args[1].evaluate(src, allocator)) {
                    .number => |rhs| .{ .bool = lhs >= rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .EQUAL_EQUAL => .{ .bool = switch (try self.args[0].evaluate(src, allocator)) {
                .number => |lhs| switch (try self.args[1].evaluate(src, allocator)) {
                    .number => |rhs| lhs == rhs,
                    else => false,
                },
                .string => |lhs| switch (try self.args[1].evaluate(src, allocator)) {
                    .string => |rhs| std.mem.eql(u8, lhs, rhs),
                    else => false,
                },
                else => false,
            } },
            .BANG_EQUAL => .{ .bool = switch (try self.args[0].evaluate(src, allocator)) {
                .number => |lhs| switch (try self.args[1].evaluate(src, allocator)) {
                    .number => |rhs| lhs != rhs,
                    else => true,
                },
                .string => |lhs| switch (try self.args[1].evaluate(src, allocator)) {
                    .string => |rhs| std.mem.eql(u8, lhs, rhs) == false,
                    else => true,
                },
                else => true,
            } },
            .PRINT => .{ .nil = try stdout.print("{any}\n", .{try self.args[0].evaluate(src, allocator)}) },
            .SEMICOLON => .{ .nil = for (self.args) |arg| {
                _ = try arg.evaluate(src, allocator);
            } },
            else => unreachable,
        };
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

    fn create(self: @This(), token: Token, lhs: ?*Node, rhs: ?*Node) ParseError!*Node {
        const n: usize = if (lhs == null) 0 else if (rhs == null) 1 else 2;
        const node = try self.allocator.create(Node);
        const args = try self.allocator.alloc(*Node, n);
        if (lhs) |arg| args[0] = arg;
        if (rhs) |arg| args[1] = arg;
        node.* = .{ .token = token, .args = args };
        return node;
    }

    pub fn peek(self: *Parser) Token {
        return self.look_ahead;
    }

    fn next(self: *Parser) Token {
        const token = self.peek();
        self.look_ahead = self.tokens.next();
        return token;
    }

    pub fn statements(self: *Parser) ParseError!*Node {
        var stmts = std.ArrayList(*Node).init(self.allocator);
        errdefer stmts.deinit();

        while (self.peek().tag != .EOF)
            try stmts.append(try self.statement());
        const node = try self.allocator.create(Node);
        node.* = .{
            .token = .{
                .tag = .SEMICOLON,
                .loc = .{ .start = 0, .end = self.tokens.buffer.len },
            },
            .args = try stmts.toOwnedSlice(),
        };
        return node;
    }

    fn statement(self: *Parser) ParseError!*Node {
        const token = self.peek();
        const stmt = switch (token.tag) {
            .PRINT => try self.create(self.next(), try self.expression(), null),
            else => try self.expression(),
        };
        return switch (self.next().tag) {
            .SEMICOLON => stmt,
            else => error.MissingSemicolon,
        };
    }

    pub fn expression(self: *Parser) ParseError!*Node {
        return self.equality();
    }

    fn equality(self: *Parser) ParseError!*Node {
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

    fn comparison(self: *Parser) ParseError!*Node {
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

    fn term(self: *Parser) ParseError!*Node {
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

    fn factor(self: *Parser) ParseError!*Node {
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

    fn unary(self: *Parser) ParseError!*Node {
        const token = self.peek();
        return switch (token.tag) {
            .BANG, .MINUS => self.create(self.next(), try self.unary(), null),
            else => self.primary(),
        };
    }

    fn primary(self: *Parser) ParseError!*Node {
        const token = self.peek();
        return switch (token.tag) {
            .STRING => switch (self.tokens.buffer[token.loc.end - 1]) {
                '"' => try self.create(self.next(), null, null),
                else => error.UnexpectedToken,
            },
            .NUMBER, .FALSE, .NIL, .TRUE => try self.create(self.next(), null, null),
            .LEFT_PAREN => blk: {
                const result = try self.create(self.next(), try self.expression(), null);
                assert(self.next().tag == .RIGHT_PAREN);
                break :blk result;
            },
            else => error.UnexpectedToken,
        };
    }
};
