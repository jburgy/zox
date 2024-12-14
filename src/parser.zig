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
    args: []const Node,

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

    pub fn peek(self: *Parser) Token {
        return self.look_ahead;
    }

    fn next(self: *Parser) Token {
        const token = self.peek();
        self.look_ahead = self.tokens.next();
        return token;
    }

    pub fn statements(self: *Parser) ParseError!Node {
        var stmts = std.ArrayList(Node).init(self.allocator);
        errdefer stmts.deinit();

        while (true) {
            switch (self.peek().tag) {
                .RIGHT_BRACE, .EOF => break,
                else => try stmts.append(try self.statement()),
            }
        }
        return Node{
            .token = .{
                .tag = .SEMICOLON,
                .loc = .{ .start = 0, .end = self.tokens.buffer.len },
            },
            .args = try stmts.toOwnedSlice(),
        };
    }

    fn statement(self: *Parser) ParseError!Node {
        const token = self.peek();
        const stmt = switch (token.tag) {
            .PRINT => Node{ .token = self.next(), .args = &[_]Node{try self.expression()} },
            .VAR => blk: {
                _ = self.next();
                const idToken = self.peek();
                switch (idToken.tag) {
                    .IDENTIFIER => {
                        const lhs = Node{ .token = self.next(), .args = &[_]Node{} };
                        break :blk switch (self.next().tag) {
                            .EQUAL => Node{ .token = token, .args = &[_]Node{ lhs, try self.expression() } },
                            .SEMICOLON => Node{ .token = token, .args = &[_]Node{lhs} },
                            else => unreachable,
                        };
                    },
                    else => unreachable,
                }
            },
            .LEFT_BRACE => blk: {
                const result = Node{ .token = self.next(), .args = &[_]Node{try self.statements()} };
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
                                        break :blk Node{ .token = root, .args = &[_]Node{ cond, head, try self.statement() } };
                                    },
                                    else => break :blk Node{ .token = root, .args = &[_]Node{ cond, head } },
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
                            .RIGHT_PAREN => Node{ .token = root, .args = &[_]Node{ cond, try self.statement() } },
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
                            .SEMICOLON => Node{ .token = self.next(), .args = &[_]Node{} },
                            .LEFT_BRACE => break :blk error.UnexpectedToken,
                            else => self.statement(),
                        };
                        const cond = try self.expression();
                        switch (self.next().tag) {
                            .SEMICOLON => switch (self.peek().tag) {
                                .RIGHT_PAREN => {
                                    _ = self.next();
                                    break :blk Node{ .token = root, .args = &[_]Node{ first, cond, try self.statement() } };
                                },
                                else => {
                                    const incr = try self.expression();
                                    break :blk switch (self.next().tag) {
                                        .RIGHT_PAREN => Node{ .token = root, .args = &[_]Node{ first, cond, try self.statement(), incr } },
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
            .FUN => blk: {
                const root = self.next();
                var args = std.ArrayList(Node).init(self.allocator);
                errdefer args.deinit();

                switch (self.peek().tag) {
                    .IDENTIFIER => try args.append(Node{ .token = self.next(), .args = &[_]Node{} }),
                    else => break :blk error.UnexpectedToken,
                }
                switch (self.next().tag) {
                    .LEFT_PAREN => {},
                    else => break :blk error.UnexpectedToken,
                }
                while (true) {
                    switch (self.peek().tag) {
                        .RIGHT_PAREN => {
                            _ = self.next();
                            break;
                        },
                        .IDENTIFIER => {
                            try args.append(Node{ .token = self.next(), .args = &[_]Node{} });
                            switch (self.next().tag) {
                                .COMMA => break,
                                .RIGHT_PAREN => break,
                                else => break :blk error.UnexpectedToken,
                            }
                        },
                        else => break :blk error.UnexpectedToken,
                    }
                }
                switch (self.next().tag) {
                    .LEFT_BRACE => try args.append(try self.statements()),
                    else => break :blk error.UnexpectedToken,
                }
                break :blk switch (self.next().tag) {
                    .RIGHT_BRACE => Node{ .token = root, .args = try args.toOwnedSlice() },
                    else => error.UnexpectedToken,
                };
            },
            else => try self.expression(),
        };
        switch (self.peek().tag) {
            .SEMICOLON => _ = self.next(),
            else => {},
        }
        return stmt;
    }

    pub fn expression(self: *Parser) ParseError!Node {
        var result = try self.equality();
        while (true) {
            result = switch (self.peek().tag) {
                .EQUAL, .OR, .AND => Node{ .token = self.next(), .args = &[_]Node{ result, try self.expression() } },
                else => break,
            };
        }
        return result;
    }

    fn equality(self: *Parser) ParseError!Node {
        var result = try self.comparison();
        while (true) {
            result = switch (self.peek().tag) {
                .EQUAL_EQUAL, .BANG_EQUAL => Node{ .token = self.next(), .args = &[_]Node{ result, try self.comparison() } },
                else => break,
            };
        }
        return result;
    }

    fn comparison(self: *Parser) ParseError!Node {
        var result = try self.term();
        while (true) {
            result = switch (self.peek().tag) {
                .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => Node{ .token = self.next(), .args = &[_]Node{ result, try self.term() } },
                else => break,
            };
        }
        return result;
    }

    fn term(self: *Parser) ParseError!Node {
        var result = try self.factor();
        while (true) {
            result = switch (self.peek().tag) {
                .MINUS, .PLUS => Node{ .token = self.next(), .args = &[_]Node{ result, try self.factor() } },
                else => break,
            };
        }
        return result;
    }

    fn factor(self: *Parser) ParseError!Node {
        var result = try self.unary();
        while (true) {
            result = switch (self.peek().tag) {
                .STAR, .SLASH => Node{ .token = self.next(), .args = &[_]Node{ result, try self.unary() } },
                else => break,
            };
        }
        return result;
    }

    fn unary(self: *Parser) ParseError!Node {
        return switch (self.peek().tag) {
            .BANG, .MINUS => Node{ .token = self.next(), .args = &[_]Node{try self.unary()} },
            else => self.call(),
        };
    }

    fn call(self: *Parser) ParseError!Node {
        const result = try self.primary();
        return switch (self.peek().tag) {
            .LEFT_PAREN => try self.finishCall(result),
            else => result,
        };
    }

    fn finishCall(self: *Parser, func: Node) ParseError!Node {
        var args = std.ArrayList(Node).init(self.allocator);
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
        return Node{ .token = self.next(), .args = try args.toOwnedSlice() };
    }

    fn primary(self: *Parser) ParseError!Node {
        const token = self.peek();
        return switch (token.tag) {
            .STRING => switch (self.tokens.buffer[token.loc.end - 1]) {
                '"' => Node{ .token = self.next(), .args = &[_]Node{} },
                else => error.UnexpectedToken,
            },
            .NUMBER, .FALSE, .NIL, .TRUE => Node{ .token = self.next(), .args = &[_]Node{} },
            .LEFT_PAREN => blk: {
                const result = Node{ .token = self.next(), .args = &[_]Node{try self.expression()} };
                break :blk switch (self.next().tag) {
                    .RIGHT_PAREN => result,
                    else => error.UnexpectedToken,
                };
            },
            .IDENTIFIER => Node{ .token = self.next(), .args = &[_]Node{} },
            else => error.UnexpectedToken,
        };
    }
};
