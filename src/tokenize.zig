const std = @import("std");

const Stream = std.io.FixedBufferStream([]const u8);
const Reader = Stream.Reader;

pub const Token = struct {
    tag: Tag,
    src: []const u8,

    pub const Tag = enum(u8) {
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

    fn maybeReserved(name: []const u8) Tag {
        return reserved.get(name) orelse .IDENTIFIER;
    }

    pub fn line(self: Token, src: []const u8) usize {
        const len = self.src.len + @intFromPtr(self.src.ptr) - @intFromPtr(src.ptr);
        return std.mem.count(u8, src[0..len], "\n") + 1;
    }

    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !bool {
        _ = fmt;
        _ = options;
        switch (self.tag) {
            .STRING => try writer.print("{s} {s} {s}\n", .{ @tagName(self.tag), self.src, self.src[1 .. self.src.len - 1] }),
            .NUMBER => {
                const value = try std.fmt.parseFloat(f64, self.src);
                var precision: ?usize = 1;
                if (std.mem.indexOfScalar(u8, self.src, '.')) |index| {
                    if (std.mem.allEqual(u8, self.src[index + 1 ..], '0') == false)
                        precision = null;
                }
                try writer.print("{s} {s} {d:.[3]}\n", .{ @tagName(self.tag), self.src, value, precision });
            },
            else => try writer.print("{s} {s} null\n", .{ @tagName(self.tag), self.src }),
        }
    }
};

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

pub fn next(stream: *Stream) Token {
    var start = stream.pos;
    var reader = stream.reader();
    var state: State = .start;
    const tag: Token.Tag = blk: while (reader.readByte() catch null) |c| {
        switch (state) {
            .start => switch (c) {
                ' ', '\n', '\t', '\r' => start = stream.pos,
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
                    stream.pos -= 1;
                    break :blk .EQUAL;
                },
            },
            .bang => switch (c) {
                '=' => break :blk .BANG_EQUAL,
                else => {
                    stream.pos -= 1;
                    break :blk .BANG;
                },
            },
            .less => switch (c) {
                '=' => break :blk .LESS_EQUAL,
                else => {
                    stream.pos -= 1;
                    break :blk .LESS;
                },
            },
            .greater => switch (c) {
                '=' => break :blk .GREATER_EQUAL,
                else => {
                    stream.pos -= 1;
                    break :blk .GREATER;
                },
            },
            .slash => switch (c) {
                '/' => state = .comment,
                else => {
                    stream.pos -= 1;
                    break :blk .SLASH;
                },
            },
            .comment => {
                start = stream.pos;
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
                    stream.pos -= 1;
                    break :blk .NUMBER;
                },
            },
            .fractional_number => switch (c) {
                '0'...'9' => {},
                else => {
                    stream.pos -= 1;
                    break :blk .NUMBER;
                },
            },
            .identifier => switch (c) {
                '0'...'9', 'A'...'Z', '_', 'a'...'z' => {},
                else => {
                    stream.pos -= 1;
                    break :blk Token.maybeReserved(stream.buffer[start..stream.pos]);
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
            .identifier => Token.maybeReserved(stream.buffer[start..stream.pos]),
            else => .EOF,
        };
    };
    return .{ .tag = tag, .src = stream.buffer[start..stream.pos] };
}

pub fn tokens(allocator: std.mem.Allocator, buffer: []const u8) std.mem.Allocator.Error![]const Token {
    var stream = std.io.fixedBufferStream(buffer);
    var result = try std.ArrayListUnmanaged(Token).initCapacity(allocator, 4);
    defer result.deinit(allocator);

    while (true) {
        const token = next(&stream);
        try result.append(allocator, token);
        if (token.tag == .EOF)
            break;
    }
    return result.toOwnedSlice(allocator);
}
