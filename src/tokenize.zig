const std = @import("std");
const testing = std.testing;

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
    ) !void {
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

fn next(stream: *Stream) ?Token {
    var start = stream.pos;
    var reader = stream.reader();
    const tag: Token.Tag = tag: {
        state: switch (State.start) {
            .start => switch (reader.readByte() catch 0) {
                0 => return null,
                ' ', '\n', '\t', '\r' => {
                    start = stream.pos;
                    continue :state .start;
                },
                '(' => break :tag .LEFT_PAREN,
                ')' => break :tag .RIGHT_PAREN,
                '{' => break :tag .LEFT_BRACE,
                '}' => break :tag .RIGHT_BRACE,
                ',' => break :tag .COMMA,
                '.' => break :tag .DOT,
                '-' => break :tag .MINUS,
                '+' => break :tag .PLUS,
                ';' => break :tag .SEMICOLON,
                '*' => break :tag .STAR,
                '=' => continue :state .equal,
                '!' => continue :state .bang,
                '<' => continue :state .less,
                '>' => continue :state .greater,
                '/' => continue :state .slash,
                '"' => continue :state .string,
                '0'...'9' => continue :state .number,
                'A'...'Z', '_', 'a'...'z' => continue :state .identifier,
                else => break :tag .INVALID,
            },
            .equal => switch (reader.readByte() catch 0) {
                '=' => break :tag .EQUAL_EQUAL,
                else => {
                    stream.pos -= 1;
                    break :tag .EQUAL;
                },
            },
            .bang => switch (reader.readByte() catch 0) {
                '=' => break :tag .BANG_EQUAL,
                else => {
                    stream.pos -= 1;
                    break :tag .BANG;
                },
            },
            .less => switch (reader.readByte() catch 0) {
                '=' => break :tag .LESS_EQUAL,
                else => {
                    stream.pos -= 1;
                    break :tag .LESS;
                },
            },
            .greater => switch (reader.readByte() catch 0) {
                '=' => break :tag .GREATER_EQUAL,
                else => {
                    stream.pos -= 1;
                    break :tag .GREATER;
                },
            },
            .slash => switch (reader.readByte() catch 0) {
                '/' => continue :state .comment,
                else => {
                    stream.pos -= 1;
                    break :tag .SLASH;
                },
            },
            .comment => {
                start = stream.pos;
                switch (reader.readByte() catch 0) {
                    0 => return null,
                    '\n' => continue :state .start,
                    else => continue :state .comment,
                }
            },
            .string => switch (reader.readByte() catch 0) {
                0 => break :tag .INVALID,
                '"' => break :tag .STRING,
                else => continue :state .string,
            },
            .number => switch (reader.readByte() catch 0) {
                0 => break :tag .NUMBER,
                '0'...'9' => continue :state .number,
                '.' => continue :state .fractional_number,
                else => {
                    stream.pos -= 1;
                    break :tag .NUMBER;
                },
            },
            .fractional_number => switch (reader.readByte() catch 0) {
                0 => break :tag .NUMBER,
                '0'...'9' => continue :state .fractional_number,
                else => {
                    stream.pos -= 1;
                    break :tag .NUMBER;
                },
            },
            .identifier => switch (reader.readByte() catch 0) {
                '0'...'9', 'A'...'Z', '_', 'a'...'z' => continue :state .identifier,
                else => |c| {
                    stream.pos -= @min(c, 1);
                    break :tag Token.maybeReserved(stream.buffer[start..stream.pos]);
                },
            },
            else => break :tag .INVALID,
        }
    };
    return .{ .tag = tag, .src = stream.buffer[start..stream.pos] };
}

pub fn tokens(allocator: std.mem.Allocator, buffer: []const u8) std.mem.Allocator.Error![]const Token {
    var stream = std.io.fixedBufferStream(buffer);
    var result = try std.ArrayListUnmanaged(Token).initCapacity(allocator, 4);
    defer result.deinit(allocator);

    while (next(&stream)) |token|
        try result.append(allocator, token);
    try result.append(allocator, .{ .tag = .EOF, .src = buffer[buffer.len..] });
    return result.toOwnedSlice(allocator);
}

test tokens {
    const buffer = "a";
    const actual = try tokens(testing.allocator, buffer);
    defer testing.allocator.free(actual);
    const expected = [_]Token{
        .{ .tag = .IDENTIFIER, .src = buffer },
        .{ .tag = .EOF, .src = buffer[buffer.len..buffer.len] },
    };
    try testing.expectEqualSlices(Token, expected[0..], actual);
}
