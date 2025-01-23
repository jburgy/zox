const std = @import("std");
const math = std.math;
const meta = std.meta;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

pub const Nil = 0x7FF2000000000000;
pub const Bool = 0x7FF4000000000000;
pub const String = 0x7FF6000000000000;
pub const Native = 0x7FF8000000000000;
pub const Function = 0x7FFA000000000000;

const Value = union {
    nil: void,
    bool: bool,
    string: []const u8,
    native: *const fn ([]const f64) f64,
    function: []const u8,
};

inline fn unsigned(comptime T: type) type {
    return meta.Int(.unsigned, @bitSizeOf(T));
}

pub fn tag(x: anytype) unsigned(@TypeOf(x)) {
    const T = @TypeOf(x);
    const U = unsigned(T);
    return @as(U, @bitCast(x)) & (((1 << (math.floatExponentBits(T) + 3)) - 1) << (math.floatMantissaBits(T) - 3));
}

fn payload(x: anytype) unsigned(@TypeOf(x)) {
    const T = @TypeOf(x);
    const U = unsigned(T);

    return @as(U, @bitCast(x)) & ((1 << (math.floatFractionalBits(T) - 3)) - 1);
}

test tag {
    inline for ([_]u64{
        0x7FF2000000000000,
        @bitCast(math.snan(f64)),
        0x7FF6000000000000,
        @bitCast(math.nan(f64)),
        0x7FFA000000000000,
        0x7FFC000000000000,
        0x7FFE000000000000,
    }) |u| {
        const x: f64 = @bitCast(u);
        try expect(math.isNan(x));
        try expectEqual(u, tag(x));
    }
}

pub fn box(x: anytype) f64 {
    return switch (@TypeOf(x)) {
        void => @bitCast(@as(u64, Nil)),
        bool => @bitCast(@as(u64, Bool) | @intFromBool(x)),
        [*:0]const u8 => @bitCast(@as(u64, String) | @intFromPtr(x)),
        []const u8 => @bitCast(@as(u64, String) | @intFromPtr(x.ptr)),
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

    try expectEqual(Nil, tag(nil));
    try expectEqual(Bool, tag(true_));
    try expectEqual(Bool, tag(false_));
}

pub fn unbox(x: f64) Value {
    return switch (tag(x)) {
        Nil => .{ .nil = {} },
        Bool => .{ .bool = payload(x) != 0 },
        String => .{ .string = std.mem.sliceTo(@as([*:0]const u8, @ptrFromInt(payload(x))), 0) },
        else => unreachable,
    };
}

test unbox {
    const expected = "Hello, world";
    try expectEqualStrings(expected, unbox(box(@as([*:0]const u8, expected))).string);
}

pub fn truthy(x: f64) bool {
    return switch (tag(x)) {
        Nil => false,
        Bool => payload(x) != 0,
        String, Native, Function => true,
        else => x != 0.0,
    };
}
