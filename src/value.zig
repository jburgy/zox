const std = @import("std");
const math = std.math;
const meta = std.meta;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

pub const Box = meta.Float(@bitSizeOf(usize));
const Tag = enum(u3) {
    number,
    nil,
    bool,
    string,
    native,
    function,
};
const shift = math.floatMantissaBits(Box) - @bitSizeOf(Tag);

fn intFromTag(comptime t: Tag) usize {
    return @as(usize, @bitCast(math.inf(Box))) | (@as(usize, @intFromEnum(t)) << shift);
}

test intFromTag {
    try expectEqual(f64, Box);
    try expectEqual(3, @bitSizeOf(Tag));
    try expectEqual(0x7FF2000000000000, intFromTag(.nil));
    try expectEqual(@as(usize, @bitCast(math.snan(Box))), intFromTag(.bool));
    try expectEqual(0x7FF6000000000000, intFromTag(.string));
    try expectEqual(@as(usize, @bitCast(math.nan(Box))), intFromTag(.native));
    try expectEqual(0x7FFA000000000000, intFromTag(.function));
}

fn tagFromInt(u: usize) Tag {
    return if (math.isFinite(@as(Box, @bitCast(u))))
        .number
    else
        @enumFromInt((u >> shift) & 0o7);
}

test tagFromInt {
    try expectEqual(.number, tagFromInt(0));
    try expectEqual(.nil, tagFromInt(0x7FF2000000000000));
    try expectEqual(.bool, tagFromInt(0x7FF4000000000000));
    try expectEqual(.string, tagFromInt(0x7FF6000000000000));
    try expectEqual(.native, tagFromInt(0x7FF8000000000000));
    try expectEqual(.function, tagFromInt(0x7FFA000000000000));
}

const Value = union {
    number: Box,
    nil: void,
    bool: bool,
    string: []const u8,
    native: *const fn ([]const Box) Box,
    function: []const u8,
};

inline fn unsigned(comptime T: type) type {
    return meta.Int(.unsigned, @bitSizeOf(T));
}

pub fn tag(x: Box) Tag {
    return tagFromInt(@bitCast(x));
}

fn payload(x: anytype) usize {
    return @as(usize, @bitCast(x)) & ((1 << shift) - 1);
}

test tag {
    inline for (meta.fields(Tag)) |f| {
        const t = @field(Tag, f.name);
        const x: Box = @bitCast(intFromTag(t));
        try expect(t == .number or math.isNan(x));
        try expectEqual(t, tag(x));
    }
}

pub fn box(x: anytype) Box {
    return switch (@TypeOf(x)) {
        comptime_float => @as(Box, x),
        Box => x,
        void => @bitCast(intFromTag(.nil)),
        bool => @bitCast(intFromTag(.bool) | @intFromBool(x)),
        [*:0]const u8 => @bitCast(intFromTag(.string) | @intFromPtr(x)),
        []const u8 => @bitCast(intFromTag(.string) | @intFromPtr(x.ptr)),
        Value => |v| switch (v) {
            .number => |y| y,
            .nil => @bitCast(intFromTag(.nil)),
            .bool => |b| @bitCast(intFromTag(.bool) | @intFromBool(b)),
            .string => |s| @bitCast(intFromTag(.string) | @intFromPtr(s.ptr)),
            else => unreachable,
        },
        else => unreachable,
    };
}

test box {
    const nil = box({});
    const true_ = box(true);
    const false_ = box(false);
    const str = box(@as([*:0]const u8, "Hello, world!"));

    try expect(math.isNan(nil));
    try expect(math.isNan(true_));
    try expect(math.isNan(false_));
    try expect(math.isNan(str));

    try expectEqual(.nil, tag(nil));
    try expectEqual(.bool, tag(true_));
    try expectEqual(.bool, tag(false_));
    try expectEqual(.string, tag(str));
}

pub fn unbox(x: Box) Value {
    return switch (tag(x)) {
        .nil => .{ .nil = {} },
        .bool => .{ .bool = payload(x) != 0 },
        .string => .{ .string = std.mem.sliceTo(@as([*:0]const u8, @ptrFromInt(payload(x))), 0) },
        else => .{ .number = x },
    };
}

test unbox {
    const expected = "Hello, world";
    try expectEqualStrings(expected, unbox(box(@as([*:0]const u8, expected))).string);
}

pub fn truthy(x: Box) bool {
    return switch (tag(x)) {
        .nil => false,
        .bool => payload(x) != 0,
        .string, .native, .function => true,
        else => x != 0.0,
    };
}
