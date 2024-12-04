const std = @import("std");

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
    };
};

pub const Tokenizer = struct {
    buffer: []const u8,
    index: usize,

    pub fn init(buffer: []const u8) Tokenizer {
        return .{ .buffer = buffer, .index = 0 };
    }

    const State = enum {
        start,
        invalid,
    };

    pub fn next(self: *Tokenizer) Token {
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
            switch (.start) {
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
                    else => break :blk .INVALID,
                },
                else => break,
            }
        } else .EOF;
        result.tag = tag;
        result.loc.end = self.index;
        return result;
    }
};
