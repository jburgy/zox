const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const math = std.math;
const mem = std.mem;
const native_endian = builtin.cpu.arch.endian();
const expect = testing.expect;
const expectEqual = testing.expectEqual;
const expectEqualStrings = testing.expectEqualStrings;
const value = @import("value.zig");

const Box = value.Box;
const N: comptime_int = @sizeOf(Box);
const Values = std.ArrayListUnmanaged(Box);
const Frame = struct { offset: usize = 0, address: usize = undefined };
const Frames = std.ArrayListUnmanaged(Frame);
const Code = std.io.FixedBufferStream([]const u8);
const Instruction = @TypeOf(end);
const InstructionPointer = @TypeOf(&end);

const Error = error{EndOfStream};

pub fn allocate_values(n: comptime_int) Values {
    return Values.initBuffer(@constCast(&[_]Box{0.0} ** n));
}

pub fn allocate_frames(n: comptime_int) Frames {
    return Frames.initBuffer(@constCast(&[_]Frame{.{}} ** n));
}

fn end(code: *Code, values: *Values, frames: *Frames) Error!void {
    _ = values;
    _ = frames;
    if (try code.getPos() != try code.getEndPos()) unreachable;
}

fn str(code: *Code, values: *Values, frames: *Frames) Error!void {
    const s = mem.sliceTo(code.buffer[code.pos..], 0);
    const n = s.len + 1; // skip past nil sentinel
    values.appendAssumeCapacity(value.box(s));
    try code.seekBy(@intCast(n));
    try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames });
}

test str {
    const expected = "Hello, World!";
    const code = .{opcode("str")} ++ expected ++ .{ 0, opcode("end") };
    var values = allocate_values(1);
    var frames = allocate_frames(0);

    try run(code, &values, &frames);
    try expectEqual(1, values.items.len);
    try expectEqualStrings(expected, value.unbox(values.pop()).string);
}

fn box(code: *Code, values: *Values, frames: *Frames) Error!void {
    var buf: [N]u8 = undefined;
    _ = try code.read(&buf);
    values.appendAssumeCapacity(@bitCast(buf));
    try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames });
}

test box {
    const code = .{opcode("box")} ++ mem.toBytes(value.box(0.0)) ++ .{opcode("box")} ++ mem.toBytes(math.nan(Box)) ++ .{opcode("end")};
    var values = allocate_values(2);
    var frames = allocate_frames(0);

    try run(&code, &values, &frames);
    try expectEqual(2, values.items.len);
    try expect(math.isNan(values.pop()));
    try expectEqual(0.0, values.pop());
}

fn pop(code: *Code, values: *Values, frames: *Frames) Error!void {
    _ = values.pop();
    try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames });
}

test pop {
    const code = [_]u8{ opcode("pop"), opcode("end") };
    var values = allocate_values(1);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(value.box(true));
    try run(&code, &values, &frames);
    try expectEqual(0, values.items.len);
}

fn dup(code: *Code, values: *Values, frames: *Frames) Error!void {
    values.appendAssumeCapacity(values.getLast());
    try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames });
}

test dup {
    const code = [_]u8{ opcode("dup"), opcode("end") };
    var values = allocate_values(2);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(1.0);
    try run(&code, &values, &frames);
    try expectEqual(2, values.items.len);
    try expectEqual(1.0, values.pop());
    try expectEqual(1.0, values.pop());
}

fn not(code: *Code, values: *Values, frames: *Frames) Error!void {
    values.appendAssumeCapacity(value.box(!value.truthy(values.pop())));
    try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames });
}

test not {
    const code = [_]u8{ opcode("not"), opcode("end") };
    var values = allocate_values(1);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(value.box(false));
    try run(&code, &values, &frames);
    try expectEqual(1, values.items.len);
    try expect(value.truthy(values.pop()));
}

fn get(code: *Code, values: *Values, frames: *Frames) Error!void {
    const reader = code.reader();
    const frame = frames.getLast();
    const i = try reader.readByte();

    values.appendAssumeCapacity(values.items[i + frame.offset]);
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames });
}

test get {
    const code = .{ opcode("get"), 0, opcode("end") };
    var values = allocate_values(2);
    var frames = allocate_frames(1);

    values.appendAssumeCapacity(value.box(@as([*:0]const u8, "Hello, world!")));
    frames.appendAssumeCapacity(.{});
    try run(&code, &values, &frames);
    try expectEqual(2, values.items.len);
    for (0..2) |_|
        try expectEqualStrings("Hello, world!", value.unbox(values.pop()).string);
}

fn set(code: *Code, values: *Values, frames: *Frames) Error!void {
    const reader = code.reader();
    const frame = frames.getLast();
    const i = try reader.readByte();
    values.items[i + frame.offset] = values.pop();

    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames });
}

test set {
    const code = .{ opcode("set"), 0, opcode("end") };
    var values = allocate_values(2);
    var frames = allocate_frames(1);

    values.appendAssumeCapacity(value.box({}));
    values.appendAssumeCapacity(value.box(@as([*:0]const u8, "Hello, world!")));
    frames.appendAssumeCapacity(.{});
    try run(&code, &values, &frames);
    try expectEqual(1, values.items.len);
    try expectEqualStrings("Hello, world!", value.unbox(values.pop()).string);
}

fn jmp(code: *Code, values: *Values, frames: *Frames) Error!void {
    const reader = code.reader();
    const n = try reader.readInt(usize, native_endian) - N;
    try code.seekBy(@intCast(n));
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames });
}

test jmp {
    const code = .{opcode("jmp")} ++ mem.toBytes(@as(usize, N)) ++ .{opcode("end")};
    var values = allocate_values(0);
    var frames = allocate_frames(0);

    try run(&code, &values, &frames);
}

fn jif(code: *Code, values: *Values, frames: *Frames) Error!void {
    const reader = code.reader();
    const n = try reader.readInt(usize, native_endian) - N;
    if (!value.truthy(values.pop()))
        try code.seekBy(@intCast(n));
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames });
}

test jif {
    const code = .{opcode("jif")} ++ mem.toBytes(@as(usize, N)) ++ .{opcode("end")};
    var values = allocate_values(1);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(value.box(false));
    try run(&code, &values, &frames);
    try expectEqual(0, values.items.len);
}

fn ebb(code: *Code, values: *Values, frames: *Frames) Error!void {
    const reader = code.reader();
    const n = try reader.readInt(isize, native_endian) + N;
    try code.seekBy(-n);
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames });
}

fn call(code: *Code, values: *Values, frames: *Frames) Error!void {
    const reader = code.reader();

    const offset = values.items.len - try reader.readByte();
    const entry: usize = @bitCast(values.items[offset]);
    frames.appendAssumeCapacity(.{ .offset = offset, .address = try code.getPos() });
    try code.seekTo(entry);
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames });
}

fn ret(code: *Code, values: *Values, frames: *Frames) Error!void {
    // assumes exactly 1 result on values
    const reader = code.reader();
    const frame = frames.pop();

    values.items[frame.offset] = values.pop();
    values.items.len = frame.offset + 1;
    try code.seekTo(frame.address);
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames });
}

const Binary = @TypeOf(add);

fn binary(comptime op: Binary) Instruction {
    return struct {
        fn wrap(code: *Code, values: *Values, frames: *Frames) Error!void {
            values.appendAssumeCapacity(op(values.pop(), values.pop()));
            try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames });
        }
    }.wrap;
}

fn add(a: Box, b: Box) Box {
    return a + b;
}

test add {
    const code = [_]u8{ opcode("add"), opcode("end") };
    var values = allocate_values(2);
    var frames = allocate_frames(0);

    for (0..2) |_| values.appendAssumeCapacity(1.0);
    try run(&code, &values, &frames);
    try expectEqual(1, values.items.len);
    try expectEqual(2.0, values.pop());
}

fn sub(a: Box, b: Box) Box {
    return a - b;
}

fn mul(a: Box, b: Box) Box {
    return a * b;
}

fn div(a: Box, b: Box) Box {
    return a / b;
}

fn compare(comptime op: math.CompareOperator) Instruction {
    return struct {
        fn wrap(code: *Code, values: *Values, frames: *Frames) Error!void {
            const b = values.pop();
            const a = values.pop();
            values.appendAssumeCapacity(value.box(switch (value.tag(a)) {
                .string => switch (value.tag(b)) {
                    .string => mem.order(u8, value.unbox(a).string, value.unbox(b).string).compare(op),
                    else => false,
                },
                else => math.compare(a, op, b),
            }));
            try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames });
        }
    }.wrap;
}

fn compareTester(a: Box, comptime op: []const u8, b: Box) !bool {
    const code = [_]u8{ opcode(op), opcode("end") };
    var values = allocate_values(2);
    var frames = allocate_frames(1);

    values.appendAssumeCapacity(a);
    values.appendAssumeCapacity(b);
    frames.appendAssumeCapacity(.{});
    try run(&code, &values, &frames);
    try expectEqual(1, values.items.len);
    return value.truthy(values.pop());
}

test compare {
    const foo = value.box(@as([*:0]const u8, "foo"));
    const bar = value.box(@as([*:0]const u8, "bar"));

    try expect(try compareTester(2.0, "lt", 3.0));
    try expect(try compareTester(2.0, "lte", 3.0));
    try expect(!try compareTester(2.0, "gt", 3.0));
    try expect(!try compareTester(2.0, "gte", 3.0));

    try expect(!try compareTester(foo, "lt", bar));
    try expect(!try compareTester(foo, "lte", bar));
    try expect(try compareTester(foo, "gt", bar));
    try expect(try compareTester(foo, "gte", bar));
}

fn equal(ok: bool) Instruction {
    return struct {
        fn wrap(code: *Code, values: *Values, frames: *Frames) Error!void {
            const b = values.pop();
            const a = values.pop();
            values.appendAssumeCapacity(value.box(switch (value.tag(a)) {
                .string => switch (value.tag(b)) {
                    .string => mem.eql(u8, value.unbox(a).string, value.unbox(b).string) == ok,
                    else => !ok,
                },
                else => (a == b) == ok,
            }));
            try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames });
        }
    }.wrap;
}

test equal {
    const nil = value.box({});
    const true_ = value.box(true);
    const false_ = value.box(false);
    const foo = value.box(@as([*:0]const u8, "foo"));
    const bar = value.box(@as([*:0]const u8, "bar"));

    try expect(!try compareTester(true_, "eql", false_));
    try expect(try compareTester(true_, "neq", false_));
    try expect(!try compareTester(2.0, "eql", 3.0));
    try expect(try compareTester(2.0, "neq", 3.0));
    try expect(!try compareTester(foo, "eql", bar));
    try expect(try compareTester(foo, "neq", bar));

    try expect(!try compareTester(true_, "eql", nil));
    try expect(!try compareTester(true_, "eql", 3.0));
    try expect(!try compareTester(true_, "eql", bar));
    try expect(try compareTester(true_, "neq", nil));
    try expect(try compareTester(true_, "neq", 3.0));
    try expect(try compareTester(true_, "neq", bar));
}

const names = std.StaticStringMap(InstructionPointer).initComptime(.{
    .{ "end", &end },
    .{ "box", &box },
    .{ "str", &str },
    .{ "pop", &pop },
    .{ "dup", &dup },
    .{ "not", &not },
    .{ "get", &get },
    .{ "set", &set },
    .{ "jmp", &jmp },
    .{ "jif", &jif },
    .{ "ebb", &ebb },
    .{ "call", &call },
    .{ "ret", &ret },
    .{ "add", &binary(add) },
    .{ "sub", &binary(sub) },
    .{ "mul", &binary(mul) },
    .{ "div", &binary(div) },
    .{ "lt", &compare(.lt) },
    .{ "lte", &compare(.lte) },
    .{ "gt", &compare(.gt) },
    .{ "gte", &compare(.gte) },
    .{ "eql", &equal(true) },
    .{ "neq", &equal(false) },
});

const instructions: []const InstructionPointer = names.values();

pub fn opcode(comptime name: []const u8) u8 {
    return @truncate(names.getIndex(name) orelse 0);
}

pub fn run(buffer: []const u8, values: *Values, frames: *Frames) Error!void {
    var code = std.io.fixedBufferStream(buffer);
    try instructions[try code.reader().readByte()](&code, values, frames);
}

pub fn disassemble(code: []const u8, writer: anytype) !void {
    var stream = std.io.fixedBufferStream(code);
    const reader = stream.reader();
    while (true) {
        const pos = try stream.getPos();
        const op = reader.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };
        try writer.print("\n{d} {s} ", .{ pos, names.keys()[op] });
        switch (op) {
            names.getIndex("str").? => try reader.streamUntilDelimiter(writer, 0, null),
            names.getIndex("box").? => {
                var buf: [N]u8 = undefined;
                std.debug.assert(N == try reader.read(&buf));
                try writer.print("{x}", .{@as(Box, @bitCast(buf))});
            },
            names.getIndex("get").?,
            names.getIndex("set").?,
            names.getIndex("call").?,
            => try writer.print("{d}", .{try reader.readByte()}),
            names.getIndex("jmp").?,
            names.getIndex("jif").?,
            names.getIndex("ebb").?,
            => try writer.print("{d}", .{try reader.readInt(usize, native_endian) - N}),
            else => {},
        }
    }
    try writer.print("\n", .{});
}
