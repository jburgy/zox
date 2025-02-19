const std = @import("std");
const testing = std.testing;
const math = std.math;
const mem = std.mem;
const native_endian = @import("builtin").cpu.arch.endian();
const expect = testing.expect;
const expectEqual = testing.expectEqual;
const expectEqualStrings = testing.expectEqualStrings;
const value = @import("value.zig");

const Box = value.Box;
const N: comptime_int = @sizeOf(Box);
const Values = std.ArrayListUnmanaged(Box);
const Frame = struct { offset: usize = 0, address: usize = undefined };
const Frames = std.ArrayListUnmanaged(Frame);
const UpValues = std.ArrayList(*Box);
const Code = std.io.FixedBufferStream([]const u8);
const Instruction = @TypeOf(end);
const InstructionPointer = @TypeOf(&end);

const Error = error{
    EndOfStream,
    OutOfMemory,
};

pub fn allocate_values(n: comptime_int) Values {
    return Values.initBuffer(@constCast(&[_]Box{0.0} ** n));
}

pub fn allocate_frames(n: comptime_int) Frames {
    return Frames.initBuffer(@constCast(&[_]Frame{.{}} ** n));
}

fn end(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    _ = values;
    _ = frames;
    _ = upvalues;
    if (try code.getPos() != try code.getEndPos()) unreachable;
}

fn str(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const s = mem.sliceTo(code.buffer[code.pos..], 0);
    const n = s.len + 1; // skip past nil sentinel
    values.appendAssumeCapacity(value.box(s));
    try code.seekBy(@intCast(n));
    try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames, upvalues });
}

test str {
    const expected = "Hello, World!";
    const code = .{opcode("str")} ++ expected ++ .{ 0, opcode("end") };
    var values = allocate_values(1);
    var frames = allocate_frames(0);

    try run(code, &values, &frames, testing.allocator);
    try expectEqual(1, values.items.len);
    try expectEqualStrings(expected, value.unbox(values.pop()).string);
}

fn box(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    var buf: [N]u8 = undefined;
    _ = try code.read(&buf);
    values.appendAssumeCapacity(@bitCast(buf));
    try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames, upvalues });
}

test box {
    const code = .{opcode("box")} ++ mem.toBytes(value.box(0)) ++ .{opcode("box")} ++ mem.toBytes(math.nan(Box)) ++ .{opcode("end")};
    var values = allocate_values(2);
    var frames = allocate_frames(0);

    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(2, values.items.len);
    try expect(math.isNan(values.pop()));
    try expectEqual(0.0, values.pop());
}

fn pop(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    _ = values.pop();
    try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames, upvalues });
}

test pop {
    const code = [_]u8{ opcode("pop"), opcode("end") };
    var values = allocate_values(1);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(value.box(true));
    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(0, values.items.len);
}

fn dup(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    values.appendAssumeCapacity(values.getLast());
    try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames, upvalues });
}

test dup {
    const code = [_]u8{ opcode("dup"), opcode("end") };
    var values = allocate_values(2);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(1.0);
    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(2, values.items.len);
    try expectEqual(1.0, values.pop());
    try expectEqual(1.0, values.pop());
}

fn not(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    values.appendAssumeCapacity(value.box(!value.truthy(values.pop())));
    try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames, upvalues });
}

test not {
    const code = [_]u8{ opcode("not"), opcode("end") };
    var values = allocate_values(1);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(value.box(false));
    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(1, values.items.len);
    try expect(value.truthy(values.pop()));
}

fn get(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const reader = code.reader();
    const i = try reader.readByte();

    values.appendAssumeCapacity(values.items[i]);
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames, upvalues });
}

test get {
    const code = .{ opcode("get"), 0, opcode("end") };
    var values = allocate_values(2);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(value.box(@as([*:0]const u8, "Hello, world!")));
    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(2, values.items.len);
    for (0..2) |_|
        try expectEqualStrings("Hello, world!", value.unbox(values.pop()).string);
}

fn set(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const reader = code.reader();
    const i = try reader.readByte();
    values.items[i] = values.pop();

    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames, upvalues });
}

test set {
    const code = .{ opcode("set"), 0, opcode("end") };
    var values = allocate_values(2);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(value.box({}));
    values.appendAssumeCapacity(value.box(@as([*:0]const u8, "Hello, world!")));
    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(1, values.items.len);
    try expectEqualStrings("Hello, world!", value.unbox(values.pop()).string);
}

fn geu(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const reader = code.reader();
    const i = try reader.readByte();
    values.appendAssumeCapacity(upvalues.items[i].*);
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames, upvalues });
}

test geu {
    var code = std.io.fixedBufferStream(&[_]u8{ opcode("geu"), 0, opcode("end") });
    var values = allocate_values(1);
    var frames = allocate_frames(0);
    var upvalues = UpValues.init(testing.allocator);
    defer upvalues.deinit();
    var val = value.box(1);

    try upvalues.append(&val);
    try instructions[try code.reader().readByte()](&code, &values, &frames, upvalues);
    try expectEqual(1, values.items.len);
    try expectEqual(1.0, values.pop());
}

fn seu(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const reader = code.reader();
    const i = try reader.readByte();
    upvalues.items[i].* = values.pop();

    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames, upvalues });
}

test seu {
    var code = std.io.fixedBufferStream(&[_]u8{ opcode("seu"), 0, opcode("end") });
    var values = allocate_values(1);
    var frames = allocate_frames(0);
    var upvalues = UpValues.init(testing.allocator);
    defer upvalues.deinit();
    var val = value.box({});

    values.appendAssumeCapacity(value.box(1));
    try upvalues.append(&val);
    try instructions[try code.reader().readByte()](&code, &values, &frames, upvalues);
    try expectEqual(0, values.items.len);
    try expectEqual(1, upvalues.items[0].*);
}

fn jmp(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const reader = code.reader();
    const n = try reader.readInt(usize, native_endian) - N;
    try code.seekBy(@intCast(n));
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames, upvalues });
}

test jmp {
    const code = .{opcode("jmp")} ++ mem.toBytes(@as(usize, N)) ++ .{opcode("end")};
    var values = allocate_values(0);
    var frames = allocate_frames(0);

    try run(&code, &values, &frames, testing.allocator);
}

fn jif(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const reader = code.reader();
    const n = try reader.readInt(usize, native_endian) - N;
    if (!value.truthy(values.pop()))
        try code.seekBy(@intCast(n));
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames, upvalues });
}

test jif {
    const code = .{opcode("jif")} ++ mem.toBytes(@as(usize, N)) ++ .{opcode("end")};
    var values = allocate_values(1);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(value.box(false));
    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(0, values.items.len);
}

fn ebb(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const reader = code.reader();
    const n = try reader.readInt(isize, native_endian) + N;
    try code.seekBy(-n);
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames, upvalues });
}

fn fun(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const reader = code.reader();
    const n = try reader.readInt(usize, native_endian) - N;
    values.appendAssumeCapacity(@bitCast(try code.getPos()));
    try code.seekBy(@intCast(n));
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames, upvalues });
}

fn env(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const reader = code.reader();
    const n = try reader.readByte();

    const allocator = upvalues.allocator;
    var new = try UpValues.initCapacity(allocator, n);
    for (0..n) |_| {
        const j: i8 = @bitCast(try reader.readByte());
        const i = @abs(j);
        new.appendAssumeCapacity(if (j < 0)
            @ptrCast((try allocator.dupe(Box, values.items[i .. i + 1])).ptr)
        else
            upvalues.items[i]);
    }
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames, new });
}

fn call(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const reader = code.reader();

    const offset = values.items.len - try reader.readByte();
    const entry: usize = @bitCast(values.items[offset]);
    frames.appendAssumeCapacity(.{ .offset = offset, .address = try code.getPos() });
    try code.seekTo(entry);
    values.items = values.items[offset..];
    values.capacity -= offset;
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames, upvalues });
}

fn ret(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    // assumes exactly 1 result on values
    const reader = code.reader();
    const frame = frames.pop();
    const offset = frame.offset;

    values.items[0] = values.pop();
    values.items = (values.items.ptr - offset)[0 .. offset + 1];
    values.capacity += offset;
    try code.seekTo(frame.address);
    try @call(.always_tail, instructions[try reader.readByte()], .{ code, values, frames, upvalues });
}

const Binary = @TypeOf(add);

fn binary(comptime op: Binary) Instruction {
    return struct {
        fn wrap(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
            values.appendAssumeCapacity(op(values.pop(), values.pop()));
            try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames, upvalues });
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
    try run(&code, &values, &frames, testing.allocator);
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
        fn wrap(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
            const b = values.pop();
            const a = values.pop();
            values.appendAssumeCapacity(value.box(switch (value.tag(a)) {
                .string => switch (value.tag(b)) {
                    .string => mem.order(u8, value.unbox(a).string, value.unbox(b).string).compare(op),
                    else => false,
                },
                else => math.compare(a, op, b),
            }));
            try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames, upvalues });
        }
    }.wrap;
}

fn compareTester(a: Box, comptime op: []const u8, b: Box) !bool {
    const code = [_]u8{ opcode(op), opcode("end") };
    var values = allocate_values(2);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(a);
    values.appendAssumeCapacity(b);
    try run(&code, &values, &frames, testing.allocator);
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
        fn wrap(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
            const b = values.pop();
            const a = values.pop();
            values.appendAssumeCapacity(value.box(switch (value.tag(a)) {
                .string => switch (value.tag(b)) {
                    .string => mem.eql(u8, value.unbox(a).string, value.unbox(b).string) == ok,
                    else => !ok,
                },
                else => (a == b) == ok,
            }));
            try @call(.always_tail, instructions[try code.reader().readByte()], .{ code, values, frames, upvalues });
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
    .{ "geu", &geu },
    .{ "seu", &seu },
    .{ "jmp", &jmp },
    .{ "jif", &jif },
    .{ "ebb", &ebb },
    .{ "fun", &fun },
    .{ "env", &env },
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

fn trace(comptime name: []const u8, comptime op: InstructionPointer) Instruction {
    return struct {
        fn wrap(code: *Code, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
            std.debug.print("{d:0>3} {s} {any}\n", .{ try code.getPos(), name, values.items });
            try @call(.always_tail, op, .{ code, values, frames, upvalues });
        }
    }.wrap;
}

const instructions: []const InstructionPointer = names.values();
// const instructions = blk: {
//     var res: [names.kvs.len]InstructionPointer = undefined;
//     for (names.keys(), names.values(), &res) |name, op, *it| {
//         it.* = &trace(name, op);
//     }
//     break :blk res;
// };

pub fn opcode(comptime name: []const u8) u8 {
    return @truncate(names.getIndex(name) orelse 0);
}

pub fn run(buffer: []const u8, values: *Values, frames: *Frames, allocator: mem.Allocator) Error!void {
    var code = std.io.fixedBufferStream(buffer);
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const upvalues = UpValues.init(arena.allocator());

    try instructions[try code.reader().readByte()](&code, values, frames, upvalues);
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
                try writer.print("{any}", .{value.unbox(@as(Box, @bitCast(buf)))});
            },
            names.getIndex("get").?,
            names.getIndex("set").?,
            names.getIndex("geu").?,
            names.getIndex("seu").?,
            names.getIndex("call").?,
            => try writer.print("{d}", .{try reader.readByte()}),
            names.getIndex("jmp").?,
            names.getIndex("jif").?,
            names.getIndex("ebb").?,
            names.getIndex("fun").?,
            => try writer.print("{d}", .{try reader.readInt(usize, native_endian) - N}),
            names.getIndex("env").? => {
                const n = try reader.readByte();
                try writer.print("[ ", .{});
                for (0..n) |_| {
                    const i: i8 = @bitCast(try reader.readByte());
                    try writer.print("{d} ", .{i});
                }
                try writer.print("]", .{});
            },
            else => {},
        }
    }
    try writer.print("\n", .{});
}
