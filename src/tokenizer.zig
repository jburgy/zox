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
        const tag: Token.Tag = blk: while (self.index < self.buffer.len) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (.start) {
                .start => switch (c) {
                    ' ', '\n', '\t', '\r' => result.loc.start = self.index + 1,
                    '(' => {
                        self.index += 1;
                        break :blk .LEFT_PAREN;
                    },
                    ')' => {
                        self.index += 1;
                        break :blk .RIGHT_PAREN;
                    },
                    '{' => {
                        self.index += 1;
                        break :blk .LEFT_BRACE;
                    },
                    '}' => {
                        self.index += 1;
                        break :blk .RIGHT_BRACE;
                    },
                    else => {
                        self.index += 1;
                        break :blk .INVALID;
                    },
                },
                else => break,
            }
        } else .EOF;
        result.tag = tag;
        result.loc.end = self.index;
        return result;
    }
};
