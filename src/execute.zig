const std = @import("std");
const testing = std.testing;
const math = std.math;
const mem = std.mem;
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
const Reader = Code.Reader;
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

fn end(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    _ = values;
    _ = frames;
    _ = upvalues;
    const stream = code.context;
    if (stream.pos != stream.buffer.len) unreachable;
}

inline fn next(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    try @call(.always_tail, instructions[try code.readByte()], .{ code, values, frames, upvalues });
}

fn str(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const stream = code.context;
    const s = mem.sliceTo(stream.buffer[stream.pos..], 0);
    const n = s.len + 1; // skip past nil sentinel
    values.appendAssumeCapacity(value.box(s));
    try stream.seekBy(@intCast(n));
    try next(code, values, frames, upvalues);
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

fn box(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    values.appendAssumeCapacity(@bitCast(mem.littleToNative(usize, try code.readInt(usize, .little))));
    try next(code, values, frames, upvalues);
}

test box {
    var buffer: [2 * N + 3]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    var writer = stream.writer();
    var values = allocate_values(2);
    var frames = allocate_frames(0);

    try writer.writeByte(opcode("box"));
    try writer.writeInt(usize, @bitCast(value.box(0)), .little);
    try writer.writeByte(opcode("box"));
    try writer.writeInt(usize, @bitCast(math.nan(Box)), .little);
    try writer.writeByte(opcode("end"));
    try run(buffer[0..], &values, &frames, testing.allocator);
    try expectEqual(2, values.items.len);
    try expect(math.isNan(values.pop()));
    try expectEqual(0.0, values.pop());
}

fn pop(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    _ = values.pop();
    try next(code, values, frames, upvalues);
}

test pop {
    const code = .{ opcode("pop"), opcode("end") };
    var values = allocate_values(1);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(value.box(true));
    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(0, values.items.len);
}

fn dup(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    values.appendAssumeCapacity(values.getLast());
    try next(code, values, frames, upvalues);
}

test dup {
    const code = .{ opcode("dup"), opcode("end") };
    var values = allocate_values(2);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(1.0);
    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(2, values.items.len);
    try expectEqual(1.0, values.pop());
    try expectEqual(1.0, values.pop());
}

fn not(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    values.appendAssumeCapacity(value.box(!value.truthy(values.pop())));
    try next(code, values, frames, upvalues);
}

test not {
    const code = .{ opcode("not"), opcode("end") };
    var values = allocate_values(1);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(value.box(false));
    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(1, values.items.len);
    try expect(value.truthy(values.pop()));
}

fn get(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const i = try code.readByte();

    values.appendAssumeCapacity(values.items[i]);
    try next(code, values, frames, upvalues);
}

test get {
    const expected = mem.bytesAsSlice(u8, "Hello, world!");
    const code = .{ opcode("get"), 0, opcode("end") };
    var values = allocate_values(2);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(value.box(expected));
    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(2, values.items.len);
    for (0..2) |_|
        try expectEqualStrings(expected, value.unbox(values.pop()).string);
}

fn set(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const i = try code.readByte();
    values.items[i] = values.pop();

    try next(code, values, frames, upvalues);
}

test set {
    const expected = mem.bytesAsSlice(u8, "Hello, world!");
    const code = .{ opcode("set"), 0, opcode("end") };
    var values = allocate_values(2);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(value.box({}));
    values.appendAssumeCapacity(value.box(expected));
    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(1, values.items.len);
    try expectEqualStrings(expected, value.unbox(values.pop()).string);
}

fn geu(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const i = try code.readByte();
    values.appendAssumeCapacity(upvalues.items[i].*);
    try next(code, values, frames, upvalues);
}

test geu {
    var stream = std.io.fixedBufferStream(&[_]u8{ opcode("geu"), 0, opcode("end") });
    var code = stream.reader();
    var values = allocate_values(1);
    var frames = allocate_frames(0);
    var upvalues = UpValues.init(testing.allocator);
    defer upvalues.deinit();
    var val = value.box(1);

    try upvalues.append(&val);
    try instructions[try code.readByte()](&code, &values, &frames, upvalues);
    try expectEqual(1, values.items.len);
    try expectEqual(1.0, values.pop());
}

fn seu(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const i = try code.readByte();
    upvalues.items[i].* = values.pop();

    try next(code, values, frames, upvalues);
}

test seu {
    var stream = std.io.fixedBufferStream(&[_]u8{ opcode("seu"), 0, opcode("end") });
    var code = stream.reader();
    var values = allocate_values(1);
    var frames = allocate_frames(0);
    var upvalues = UpValues.init(testing.allocator);
    defer upvalues.deinit();
    var val = value.box({});

    values.appendAssumeCapacity(value.box(1));
    try upvalues.append(&val);
    try instructions[try code.readByte()](&code, &values, &frames, upvalues);
    try expectEqual(0, values.items.len);
    try expectEqual(1, upvalues.items[0].*);
}

fn jmp(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const n = try code.readInt(i32, .little);
    try code.context.seekBy(@intCast(n));
    try next(code, values, frames, upvalues);
}

test jmp {
    const code = .{opcode("jmp")} ++ mem.toBytes(@as(i32, 0)) ++ .{opcode("end")};
    var values = allocate_values(0);
    var frames = allocate_frames(0);

    try run(&code, &values, &frames, testing.allocator);
}

fn jif(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const n = try code.readInt(i32, .little);
    if (!value.truthy(values.pop()))
        try code.context.seekBy(@intCast(n));
    try next(code, values, frames, upvalues);
}

test jif {
    const code = .{opcode("jif")} ++ mem.toBytes(@as(i32, 0)) ++ .{opcode("end")};
    var values = allocate_values(1);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(value.box(false));
    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(0, values.items.len);
}

fn fun(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const n = try code.readInt(i32, .little);
    const stream = code.context;
    values.appendAssumeCapacity(@bitCast(stream.pos));
    try stream.seekBy(@intCast(n));
    try next(code, values, frames, upvalues);
}

fn env(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const n = try code.readByte();

    const allocator = upvalues.allocator;
    var new = try UpValues.initCapacity(allocator, n);
    for (0..n) |_| {
        const j: i8 = @bitCast(try code.readByte());
        const i = @abs(j);
        new.appendAssumeCapacity(if (j < 0) blk: {
            const p = try allocator.create(Box);
            p.* = values.items[i];
            break :blk p;
        } else upvalues.items[i]);
    }
    try next(code, values, frames, new);
}

fn call(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    const offset = values.items.len - try code.readByte();
    const entry: usize = @bitCast(values.items[offset]);
    const stream = code.context;
    frames.appendAssumeCapacity(.{ .offset = offset, .address = stream.pos });
    try stream.seekTo(entry);
    values.items = values.items[offset..];
    values.capacity -= offset;
    try next(code, values, frames, upvalues);
}

fn ret(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
    // assumes exactly 1 result on values
    const frame = frames.pop();
    const offset = frame.offset;

    values.items[0] = values.pop();
    values.items = (values.items.ptr - offset)[0 .. offset + 1];
    values.capacity += offset;
    try code.context.seekTo(frame.address);
    try next(code, values, frames, upvalues);
}

const Binary = @TypeOf(add);

fn binary(comptime op: Binary) Instruction {
    return struct {
        fn wrap(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
            values.appendAssumeCapacity(op(values.pop(), values.pop()));
            try next(code, values, frames, upvalues);
        }
    }.wrap;
}

fn add(a: Box, b: Box) Box {
    return a + b;
}

test add {
    const code = .{ opcode("add"), opcode("end") };
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
        fn wrap(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
            const b = values.pop();
            const a = values.pop();
            values.appendAssumeCapacity(value.box(switch (value.unbox(a)) {
                .string => |s| switch (value.unbox(b)) {
                    .string => |t| mem.order(u8, s, t).compare(op),
                    else => false,
                },
                else => math.compare(a, op, b),
            }));
            try next(code, values, frames, upvalues);
        }
    }.wrap;
}

fn compareTester(a: Box, comptime op: []const u8, b: Box) !bool {
    const code = .{ opcode(op), opcode("end") };
    var values = allocate_values(2);
    var frames = allocate_frames(0);

    values.appendAssumeCapacity(a);
    values.appendAssumeCapacity(b);
    try run(&code, &values, &frames, testing.allocator);
    try expectEqual(1, values.items.len);
    return value.truthy(values.pop());
}

test compare {
    const foo = value.box(mem.bytesAsSlice(u8, "foo"));
    const bar = value.box(mem.bytesAsSlice(u8, "bar"));

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
        fn wrap(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
            const b = values.pop();
            const a = values.pop();
            values.appendAssumeCapacity(value.box(switch (value.unbox(a)) {
                .string => |s| switch (value.unbox(b)) {
                    .string => |t| mem.eql(u8, s, t) == ok,
                    else => !ok,
                },
                else => (a == b) == ok,
            }));
            try next(code, values, frames, upvalues);
        }
    }.wrap;
}

test equal {
    const nil = value.box({});
    const true_ = value.box(true);
    const false_ = value.box(false);
    const foo = value.box(mem.bytesAsSlice(u8, "foo"));
    const bar = value.box(mem.bytesAsSlice(u8, "bar"));

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
        fn wrap(code: *Reader, values: *Values, frames: *Frames, upvalues: UpValues) Error!void {
            std.debug.print("{d:0>3} {s} {any}\n", .{ code.context.pos, name, values.items });
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
    var stream = std.io.fixedBufferStream(buffer);
    var code = stream.reader();
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const upvalues = UpValues.init(arena.allocator());

    try instructions[try code.readByte()](&code, values, frames, upvalues);
}

pub fn disassemble(code: []const u8, writer: anytype) !void {
    var stream = std.io.fixedBufferStream(code);
    const reader = stream.reader();
    while (true) {
        const pos = stream.pos;
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
            names.getIndex("fun").?,
            => try writer.print("{d}", .{try reader.readInt(i32, .little)}),
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
