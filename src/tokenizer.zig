const std = @import("std");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

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
    };
};

pub const Tokenizer = struct {
    buffer: []const u8,
    index: usize,

    pub fn dump(self: *Tokenizer, token: Token) !bool {
        const src = self.buffer[token.loc.start..token.loc.end];
        var lexical_error = false;

        if (token.tag == .INVALID) {
            const line = std.mem.count(u8, self.buffer[0..token.loc.end], "\n");
            try stderr.print("[line {d}] Error: Unexpected character: {s}\n", .{ line + 1, src });
            lexical_error = true;
        } else if (token.tag == .STRING) {
            const valid = self.buffer[token.loc.end - 1] == '"';
            if (valid) {
                try stdout.print("{s} {s} {s}\n", .{ @tagName(token.tag), src, src[1 .. src.len - 1] });
            } else {
                const line = std.mem.count(u8, self.buffer[0..token.loc.end], "\n");
                try stderr.print("[line {d}] Error: Unterminated string.\n", .{line + 1});
                lexical_error = true;
            }
        } else {
            try stdout.print("{s} {s} null\n", .{ @tagName(token.tag), src });
        }
        return lexical_error;
    }

    pub fn init(buffer: []const u8) Tokenizer {
        return .{ .buffer = buffer, .index = 0 };
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
        invalid,
    };

    pub fn next(self: *Tokenizer) Token {
        var state: State = .start;
        var result: Token = .{
            .tag = undefined,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };
        const tag: Token.Tag = blk: while (self.index < self.buffer.len) {
            const c = self.buffer[self.index];
            self.index += 1;
            switch (state) {
                .start => switch (c) {
                    ' ', '\n', '\t', '\r' => result.loc.start = self.index,
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
                    result.loc.start = self.index;
                    switch (c) {
                        '\n' => state = .start,
                        else => {},
                    }
                },
                .string => switch (c) {
                    '"' => break :blk .STRING,
                    else => {},
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
                else => .EOF,
            };
        };
        result.tag = tag;
        result.loc.end = self.index;
        return result;
    }
};
