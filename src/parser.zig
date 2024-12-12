const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Tokenizer = tokenizer.Tokenizer;

const ParseError = error{
    OutOfMemory,
    UnexpectedToken,
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
            .FALSE, .NIL, .TRUE, .IDENTIFIER => try writer.print("{s}", .{slice}),
            .SEMICOLON => {
                try self.args[0].emit(src, writer);
                for (self.args[1..]) |arg| {
                    try writer.print("\n", .{});
                    try arg.emit(src, writer);
                }
            },
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

    fn create(self: @This(), token: Token, args: anytype) ParseError!*Node {
        const ArgsType = @TypeOf(args);
        const args_type_info = @typeInfo(ArgsType);
        if (args_type_info != .Struct) {
            @compileError("expected tuple or struct argument, found " ++ @typeName(ArgsType));
        }
        const fields_info = args_type_info.Struct.fields;
        const node = try self.allocator.create(Node);
        node.* = .{ .token = token, .args = init: {
            var nargs = try self.allocator.alloc(*Node, fields_info.len);
            inline for (fields_info, 0..) |field, i| {
                nargs[i] = @field(args, field.name);
            }
            break :init nargs;
        } };
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

        while (true) {
            switch (self.peek().tag) {
                .RIGHT_BRACE, .EOF => break,
                else => try stmts.append(try self.statement()),
            }
        }
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
            .PRINT => try self.create(self.next(), .{try self.expression()}),
            .VAR => blk: {
                _ = self.next();
                const idToken = self.peek();
                switch (idToken.tag) {
                    .IDENTIFIER => {
                        const lhs = try self.create(self.next(), .{});
                        break :blk switch (self.next().tag) {
                            .EQUAL => try self.create(token, .{ lhs, try self.expression() }),
                            .SEMICOLON => try self.create(token, .{lhs}),
                            else => unreachable,
                        };
                    },
                    else => unreachable,
                }
            },
            .LEFT_BRACE => blk: {
                const result = try self.create(self.next(), .{try self.statements()});
                break :blk switch (self.next().tag) {
                    .RIGHT_BRACE => result,
                    else => error.UnexpectedToken,
                };
            },
            .IF => blk: {
                const root = self.next();
                switch (self.next().tag) {
                    .LEFT_PAREN => {
                        const cond = try self.expression();
                        switch (self.next().tag) {
                            .RIGHT_PAREN => {
                                const head = try self.statement();
                                switch (self.peek().tag) {
                                    .ELSE => {
                                        _ = self.next();
                                        break :blk try self.create(root, .{ cond, head, try self.statement() });
                                    },
                                    else => break :blk try self.create(root, .{ cond, head }),
                                }
                            },
                            else => break :blk error.UnexpectedToken,
                        }
                    },
                    else => break :blk error.UnexpectedToken,
                }
            },
            .WHILE => blk: {
                const root = self.next();
                switch (self.next().tag) {
                    .LEFT_PAREN => {
                        const cond = try self.expression();
                        break :blk switch (self.next().tag) {
                            .RIGHT_PAREN => self.create(root, .{ cond, try self.statement() }),
                            else => error.UnexpectedToken,
                        };
                    },
                    else => break :blk error.UnexpectedToken,
                }
            },
            .FOR => blk: {
                const root = self.next();
                switch (self.next().tag) {
                    .LEFT_PAREN => {
                        const first = try switch (self.peek().tag) {
                            .SEMICOLON => self.create(self.next(), .{}),
                            .LEFT_BRACE => break :blk error.UnexpectedToken,
                            else => self.statement(),
                        };
                        const cond = try self.expression();
                        switch (self.next().tag) {
                            .SEMICOLON => switch (self.peek().tag) {
                                .RIGHT_PAREN => {
                                    _ = self.next();
                                    break :blk try self.create(root, .{ first, cond, try self.statement() });
                                },
                                else => {
                                    const incr = try self.expression();
                                    break :blk switch (self.next().tag) {
                                        .RIGHT_PAREN => try self.create(root, .{ first, cond, try self.statement(), incr }),
                                        else => error.UnexpectedToken,
                                    };
                                },
                            },
                            else => break :blk error.UnexpectedToken,
                        }
                    },
                    else => break :blk error.UnexpectedToken,
                }
            },
            else => try self.expression(),
        };
        switch (self.peek().tag) {
            .SEMICOLON => _ = self.next(),
            else => {},
        }
        return stmt;
    }

    pub fn expression(self: *Parser) ParseError!*Node {
        var result = try self.equality();
        while (true) {
            result = switch (self.peek().tag) {
                .EQUAL, .OR, .AND => try self.create(self.next(), .{ result, try self.expression() }),
                else => break,
            };
        }
        return result;
    }

    fn equality(self: *Parser) ParseError!*Node {
        var result = try self.comparison();
        while (true) {
            result = switch (self.peek().tag) {
                .EQUAL_EQUAL, .BANG_EQUAL => try self.create(self.next(), .{ result, try self.comparison() }),
                else => break,
            };
        }
        return result;
    }

    fn comparison(self: *Parser) ParseError!*Node {
        var result = try self.term();
        while (true) {
            result = switch (self.peek().tag) {
                .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => try self.create(self.next(), .{ result, try self.term() }),
                else => break,
            };
        }
        return result;
    }

    fn term(self: *Parser) ParseError!*Node {
        var result = try self.factor();
        while (true) {
            result = switch (self.peek().tag) {
                .MINUS, .PLUS => try self.create(self.next(), .{ result, try self.factor() }),
                else => break,
            };
        }
        return result;
    }

    fn factor(self: *Parser) ParseError!*Node {
        var result = try self.unary();
        while (true) {
            result = switch (self.peek().tag) {
                .STAR, .SLASH => try self.create(self.next(), .{ result, try self.unary() }),
                else => break,
            };
        }
        return result;
    }

    fn unary(self: *Parser) ParseError!*Node {
        return switch (self.peek().tag) {
            .BANG, .MINUS => self.create(self.next(), .{try self.unary()}),
            else => self.call(),
        };
    }

    fn call(self: *Parser) ParseError!*Node {
        const result = try self.primary();
        return switch (self.peek().tag) {
            .LEFT_PAREN => try self.finishCall(result),
            else => result,
        };
    }

    fn finishCall(self: *Parser, func: *Node) ParseError!*Node {
        var args = std.ArrayList(*Node).init(self.allocator);
        errdefer args.deinit();

        _ = self.next(); // consume '('
        try args.append(func);
        while (true) {
            switch (self.peek().tag) {
                .RIGHT_PAREN => break,
                else => {
                    try args.append(try self.expression());
                    switch (self.peek().tag) {
                        .COMMA => _ = self.next(),
                        else => {},
                    }
                },
            }
        }
        const node = try self.allocator.create(Node);
        node.* = .{ .token = self.next(), .args = try args.toOwnedSlice() };
        return node;
    }

    fn primary(self: *Parser) ParseError!*Node {
        const token = self.peek();
        return switch (token.tag) {
            .STRING => switch (self.tokens.buffer[token.loc.end - 1]) {
                '"' => try self.create(self.next(), .{}),
                else => error.UnexpectedToken,
            },
            .NUMBER, .FALSE, .NIL, .TRUE => try self.create(self.next(), .{}),
            .LEFT_PAREN => blk: {
                const result = try self.create(self.next(), .{try self.expression()});
                break :blk switch (self.next().tag) {
                    .RIGHT_PAREN => result,
                    else => error.UnexpectedToken,
                };
            },
            .IDENTIFIER => try self.create(self.next(), .{}),
            else => error.UnexpectedToken,
        };
    }
};
