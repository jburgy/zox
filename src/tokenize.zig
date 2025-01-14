const std = @import("std");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

pub const Token = struct {
    tag: Tag,
    src: []const u8,

    pub const Tag = enum {
        INVALID,
        EOF,
        LEFT_PAREN,
        RIGHT_PAREN,
        LEFT_BRACE,
        RIGHT_BRACE,
        COMMA,
        DOT,
        MINUS,
        PLUS,
        SEMICOLON,
        STAR,
        EQUAL,
        EQUAL_EQUAL,
        BANG,
        BANG_EQUAL,
        LESS,
        LESS_EQUAL,
        GREATER,
        GREATER_EQUAL,
        SLASH,
        STRING,
        NUMBER,
        IDENTIFIER,
        AND,
        CLASS,
        ELSE,
        FALSE,
        FOR,
        FUN,
        IF,
        NIL,
        OR,
        PRINT,
        RETURN,
        SUPER,
        THIS,
        TRUE,
        VAR,
        WHILE,
    };

    const reserved = std.StaticStringMap(Tag).initComptime(.{
        .{ "and", .AND },
        .{ "class", .CLASS },
        .{ "else", .ELSE },
        .{ "false", .FALSE },
        .{ "for", .FOR },
        .{ "fun", .FUN },
        .{ "if", .IF },
        .{ "nil", .NIL },
        .{ "or", .OR },
        .{ "print", .PRINT },
        .{ "return", .RETURN },
        .{ "super", .SUPER },
        .{ "this", .THIS },
        .{ "true", .TRUE },
        .{ "var", .VAR },
        .{ "while", .WHILE },
    });

    fn maybeReserved(name: []const u8) ?Tag {
        return reserved.get(name);
    }

    pub fn line(self: @This(), src: []const u8) usize {
        const len = self.src.len + @intFromPtr(self.src.ptr) - @intFromPtr(src.ptr);
        return std.mem.count(u8, src[0..len], "\n") + 1;
    }
};

const Tokenizer = struct {
    buffer: []const u8,
    index: usize = 0,

    pub fn dump(self: *Tokenizer, token: Token) !bool {
        const src = token.src;
        var lexical_error = false;

        switch (token.tag) {
            .INVALID => {
                try stderr.print("[line {d}] Error: Unexpected character: {s}\n", .{ token.line(self.buffer), src });
                lexical_error = true;
            },
            .STRING => {
                const valid = self.buffer[token.loc.end - 1] == '"';
                if (valid) {
                    try stdout.print("{s} {s} {s}\n", .{ @tagName(token.tag), src, src[1 .. src.len - 1] });
                } else {
                    try stderr.print("[line {d}] Error: Unterminated string.\n", .{token.line(self.buffer)});
                    lexical_error = true;
                }
            },
            .NUMBER => {
                const value = try std.fmt.parseFloat(f64, src);
                var precision: ?usize = 1;
                if (std.mem.indexOfScalar(u8, src, '.')) |index| {
                    if (std.mem.allEqual(u8, src[index + 1 ..], '0') == false)
                        precision = null;
                }
                try stdout.print("{s} {s} {d:.[3]}\n", .{ @tagName(token.tag), src, value, precision });
            },
            else => try stdout.print("{s} {s} null\n", .{ @tagName(token.tag), src }),
        }
        return lexical_error;
    }

    fn maybeReserved(self: *Tokenizer, start: usize) Token.Tag {
        return Token.maybeReserved(self.buffer[start..self.index]) orelse .IDENTIFIER;
    }

    const State = enum {
        start,
        equal,
        bang,
        less,
        greater,
        slash,
        comment,
        string,
        number,
        fractional_number,
        identifier,
        invalid,
    };

    pub fn next(self: *Tokenizer) Token {
        var start = self.index;
        var state: State = .start;
        const tag: Token.Tag = blk: while (self.index < self.buffer.len) {
            const c = self.buffer[self.index];
            self.index += 1;
            switch (state) {
                .start => switch (c) {
                    ' ', '\n', '\t', '\r' => start = self.index,
                    '(' => break :blk .LEFT_PAREN,
                    ')' => break :blk .RIGHT_PAREN,
                    '{' => break :blk .LEFT_BRACE,
                    '}' => break :blk .RIGHT_BRACE,
                    ',' => break :blk .COMMA,
                    '.' => break :blk .DOT,
                    '-' => break :blk .MINUS,
                    '+' => break :blk .PLUS,
                    ';' => break :blk .SEMICOLON,
                    '*' => break :blk .STAR,
                    '=' => state = .equal,
                    '!' => state = .bang,
                    '<' => state = .less,
                    '>' => state = .greater,
                    '/' => state = .slash,
                    '"' => state = .string,
                    '0'...'9' => state = .number,
                    'A'...'Z', '_', 'a'...'z' => state = .identifier,
                    else => break :blk .INVALID,
                },
                .equal => switch (c) {
                    '=' => break :blk .EQUAL_EQUAL,
                    else => {
                        self.index -= 1;
                        break :blk .EQUAL;
                    },
                },
                .bang => switch (c) {
                    '=' => break :blk .BANG_EQUAL,
                    else => {
                        self.index -= 1;
                        break :blk .BANG;
                    },
                },
                .less => switch (c) {
                    '=' => break :blk .LESS_EQUAL,
                    else => {
                        self.index -= 1;
                        break :blk .LESS;
                    },
                },
                .greater => switch (c) {
                    '=' => break :blk .GREATER_EQUAL,
                    else => {
                        self.index -= 1;
                        break :blk .GREATER;
                    },
                },
                .slash => switch (c) {
                    '/' => state = .comment,
                    else => {
                        self.index -= 1;
                        break :blk .SLASH;
                    },
                },
                .comment => {
                    start = self.index;
                    switch (c) {
                        '\n' => state = .start,
                        else => {},
                    }
                },
                .string => switch (c) {
                    '"' => break :blk .STRING,
                    else => {},
                },
                .number => switch (c) {
                    '0'...'9' => {},
                    '.' => state = .fractional_number,
                    else => {
                        self.index -= 1;
                        break :blk .NUMBER;
                    },
                },
                .fractional_number => switch (c) {
                    '0'...'9' => {},
                    else => {
                        self.index -= 1;
                        break :blk .NUMBER;
                    },
                },
                .identifier => switch (c) {
                    '0'...'9', 'A'...'Z', '_', 'a'...'z' => {},
                    else => {
                        self.index -= 1;
                        break :blk self.maybeReserved(start);
                    },
                },
                else => break :blk .INVALID,
            }
        } else {
            break :blk switch (state) {
                .equal => .EQUAL,
                .bang => .BANG,
                .less => .LESS,
                .greater => .GREATER,
                .slash => .SLASH,
                .string => .STRING,
                .number => .NUMBER,
                .fractional_number => .NUMBER,
                .identifier => self.maybeReserved(start),
                else => .EOF,
            };
        };
        return .{ .tag = tag, .src = self.buffer[start..self.index] };
    }
};

pub fn tokens(allocator: std.mem.Allocator, buffer: []const u8) std.mem.Allocator.Error![]const Token {
    var tokenizer = Tokenizer{ .buffer = buffer };
    var result = try std.ArrayListUnmanaged(Token).initCapacity(allocator, 4);
    defer result.deinit(allocator);

    while (true) {
        const token = tokenizer.next();
        try result.append(allocator, token);
        if (token.tag == .EOF)
            break;
    }
    return result.toOwnedSlice(allocator);
}
