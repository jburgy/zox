const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const math = std.math;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const native_endian = builtin.cpu.arch.endian();
const expect = testing.expect;
const expectEqual = testing.expectEqual;
const expectEqualStrings = testing.expectEqualStrings;
const value = @import("value.zig");

const Box = value.Box;
const N: comptime_int = @sizeOf(Box);
pub const Stack = std.ArrayList(Box);
const Instruction = @TypeOf(end);
const InstructionPointer = @TypeOf(&end);

pub const Values = std.SinglyLinkedList([]Box);
pub const Index = packed struct(u32) { depth: u8, index: u24 };

fn test_stack(n: comptime_int) Stack {
    var stack = std.ArrayListUnmanaged(Box).initBuffer(@constCast(&[_]Box{0.0} ** n));
    return stack.toManaged(testing.allocator);
}

fn end(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    _ = stack;
    _ = values;
    if (code.len != 0) unreachable;
}

fn str(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    const s = mem.sliceTo(code, 0);
    const n = s.len + 1; // skip past nil sentinel
    stack.appendAssumeCapacity(value.box(s));
    try @call(.always_tail, instructions[code[n]], .{ code[n + 1 ..], stack, values });
}

test str {
    const expected = "Hello, World!";
    const code = .{opcode("str")} ++ expected ++ .{ 0, opcode("end") };
    var stack = test_stack(1);
    var values = Values{};

    try run(code, &stack, &values);
    try expectEqual(1, stack.items.len);
    try expectEqualStrings(expected, value.unbox(stack.pop()).string);
}

fn box(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    stack.appendAssumeCapacity(@bitCast(code[0..N].*));
    try @call(.always_tail, instructions[code[N]], .{ code[N + 1 ..], stack, values });
}

test box {
    const code = .{opcode("box")} ++ mem.toBytes(value.box(0.0)) ++ .{opcode("box")} ++ mem.toBytes(math.nan(Box)) ++ .{opcode("end")};
    var stack = test_stack(2);
    var values = Values{};

    try run(&code, &stack, &values);
    try expectEqual(2, stack.items.len);
    try expect(math.isNan(stack.pop()));
    try expectEqual(0.0, stack.pop());
}

fn pop(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    _ = stack.pop();
    try @call(.always_tail, instructions[code[0]], .{ code[1..], stack, values });
}

test pop {
    const code = [_]u8{ opcode("pop"), opcode("end") };
    var stack = test_stack(1);
    var values = Values{};

    stack.appendAssumeCapacity(value.box(true));
    try run(&code, &stack, &values);
    try expectEqual(0, stack.items.len);
}

fn dup(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    stack.appendAssumeCapacity(stack.getLast());
    try @call(.always_tail, instructions[code[0]], .{ code[1..], stack, values });
}

test dup {
    const code = [_]u8{ opcode("dup"), opcode("end") };
    var stack = test_stack(2);
    var values = Values{};

    stack.appendAssumeCapacity(1.0);
    try run(&code, &stack, &values);
    try expectEqual(2, stack.items.len);
    try expectEqual(1.0, stack.pop());
    try expectEqual(1.0, stack.pop());
}

fn not(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    stack.appendAssumeCapacity(value.box(!value.truthy(stack.pop())));
    try @call(.always_tail, instructions[code[0]], .{ code[1..], stack, values });
}

test not {
    const code = [_]u8{ opcode("not"), opcode("end") };
    var stack = test_stack(1);
    var values = Values{};

    stack.appendAssumeCapacity(value.box(false));
    try run(&code, &stack, &values);
    try expectEqual(1, stack.items.len);
    try expect(value.truthy(stack.pop()));
}

fn get(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    const n = @sizeOf(Index);
    const i: Index = @bitCast(code[0..n].*);

    var p = values.first;
    for (0..i.depth) |_| p = p.?.next;
    stack.appendAssumeCapacity(p.?.data[i.index]);
    try @call(.always_tail, instructions[code[n]], .{ code[n + 1 ..], stack, values });
}

test get {
    const code = .{opcode("get")} ++ mem.toBytes(Index{ .depth = 0, .index = 0 }) ++ .{opcode("end")};
    var stack = test_stack(1);
    var values = Values{};
    var locals = [_]Box{value.box(@as([*:0]const u8, "Hello, world!"))};
    var scope = Values.Node{ .data = locals[0..] };

    values.prepend(&scope);
    try run(&code, &stack, &values);
    try expectEqual(1, stack.items.len);
    try expectEqualStrings("Hello, world!", value.unbox(stack.pop()).string);
}

fn set(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    const n = @sizeOf(Index);
    const i: Index = @bitCast(code[0..n].*);

    var p = values.first;
    for (0..i.depth) |_| p = p.?.next;
    p.?.data[i.index] = stack.pop();

    try @call(.always_tail, instructions[code[n]], .{ code[n + 1 ..], stack, values });
}

test set {
    const code = .{opcode("set")} ++ mem.toBytes(Index{ .depth = 0, .index = 0 }) ++ .{opcode("end")};
    var stack = test_stack(1);
    var values = Values{};
    var locals = [_]Box{value.box({})};
    var scope = Values.Node{ .data = locals[0..] };

    values.prepend(&scope);
    stack.appendAssumeCapacity(value.box(@as([*:0]const u8, "Hello, world!")));
    try run(&code, &stack, &values);
    try expectEqual(0, stack.items.len);
    try expectEqualStrings("Hello, world!", value.unbox(locals[0]).string);
}

fn new(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    var scope = try stack.allocator.create(Values.Node);
    scope.data = try stack.allocator.alloc(Box, code[0]);
    values.prepend(scope);
    try @call(.always_tail, instructions[code[1]], .{ code[2..], stack, values });
}

fn del(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    const scope = values.popFirst().?;
    stack.allocator.free(scope.data);
    stack.allocator.destroy(scope);
    try @call(.always_tail, instructions[code[0]], .{ code[1..], stack, values });
}

fn jmp(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    const n = mem.readInt(usize, code[0..N], native_endian);
    try @call(.always_tail, instructions[code[n]], .{ code[n + 1 ..], stack, values });
}

test jmp {
    const code = .{opcode("jmp")} ++ mem.toBytes(@as(usize, N)) ++ .{opcode("end")};
    var stack = test_stack(0);
    var values = Values{};
    try run(&code, &stack, &values);
}

fn jif(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    const n = if (value.truthy(stack.pop())) N else mem.readInt(usize, code[0..N], native_endian);
    try @call(.always_tail, instructions[code[n]], .{ code[n + 1 ..], stack, values });
}

test jif {
    const code = .{opcode("jif")} ++ mem.toBytes(@as(usize, N)) ++ .{opcode("end")};
    var stack = test_stack(1);
    var values = Values{};

    stack.appendAssumeCapacity(value.box(false));
    try run(&code, &stack, &values);
    try expectEqual(0, stack.items.len);
}

fn ebb(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    const n = mem.readInt(usize, code[0..N], native_endian);
    const p = (code.ptr - n)[0 .. code.len + n];
    try @call(.always_tail, instructions[p[0]], .{ p[1..], stack, values });
}

fn call(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void { // return address
    const entry: usize = @bitCast(stack.pop()); // function address
    const index: usize = @bitCast(stack.items[stack.items.len - code[0]]); // return address
    const offset = index - entry; // always +ve (can only call function _after_ they were defined)
    const next = (code.ptr - offset)[0 .. code.len + offset];
    try @call(.always_tail, instructions[next[0]], .{ next[1..], stack, values });
}

fn ret(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    // assumes exactly 1 result on stack
    const index: usize = @bitCast(stack.orderedRemove(stack.items.len - 2));
    const entry = mem.readInt(usize, code[0..N], native_endian);
    const offset = index + N + 1 - entry;
    try @call(.always_tail, instructions[code[offset]], .{ code[offset + 1 ..], stack, values });
}

const Binary = @TypeOf(add);

fn binary(comptime op: Binary) Instruction {
    return struct {
        fn wrap(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
            stack.appendAssumeCapacity(op(stack.pop(), stack.pop()));
            try @call(.always_tail, instructions[code[0]], .{ code[1..], stack, values });
        }
    }.wrap;
}

fn add(a: Box, b: Box) Box {
    return a + b;
}

test add {
    const code = [_]u8{ opcode("add"), opcode("end") };
    var stack = test_stack(2);
    var values = Values{};

    for (0..2) |_| stack.appendAssumeCapacity(1.0);
    try run(&code, &stack, &values);
    try expectEqual(1, stack.items.len);
    try expectEqual(2.0, stack.pop());
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
        fn wrap(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
            const b = stack.pop();
            const a = stack.pop();
            stack.appendAssumeCapacity(value.box(switch (value.tag(a)) {
                .string => switch (value.tag(b)) {
                    .string => mem.order(u8, value.unbox(a).string, value.unbox(b).string).compare(op),
                    else => false,
                },
                else => math.compare(a, op, b),
            }));
            try @call(.always_tail, instructions[code[0]], .{ code[1..], stack, values });
        }
    }.wrap;
}

fn compareTester(a: Box, comptime op: []const u8, b: Box) !bool {
    const code = [_]u8{ opcode(op), opcode("end") };
    var stack = test_stack(2);
    var values = Values{};

    stack.appendAssumeCapacity(a);
    stack.appendAssumeCapacity(b);
    try run(&code, &stack, &values);
    try expectEqual(1, stack.items.len);
    return value.truthy(stack.pop());
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
        fn wrap(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
            const b = stack.pop();
            const a = stack.pop();
            stack.appendAssumeCapacity(value.box(switch (value.tag(a)) {
                .string => switch (value.tag(b)) {
                    .string => mem.eql(u8, value.unbox(a).string, value.unbox(b).string) == ok,
                    else => !ok,
                },
                else => (a == b) == ok,
            }));
            try @call(.always_tail, instructions[code[0]], .{ code[1..], stack, values });
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
    .{ "new", &new },
    .{ "del", &del },
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

pub fn run(code: []const u8, stack: *Stack, values: *Values) Allocator.Error!void {
    try instructions[code[0]](code[1..], stack, values);
}

pub fn disassemble(code: []const u8, writer: anytype) !void {
    var i: usize = 0;
    while (i < code.len) {
        const op = code[i];
        i += 1;
        switch (op) {
            names.getIndex("str").? => {
                const s = mem.sliceTo(code[i..], 0);
                try writer.print("{d} \"{s}\"\n", .{ i, s });
                i += s.len + 1;
            },
            names.getIndex("box").? => {
                const val: Box = @bitCast(code[i..][0..N].*);
                try writer.print("{d} {x}\n", .{ i, val });
                i += N;
            },
            names.getIndex("get").?, names.getIndex("set").? => {
                const n = @sizeOf(Index);
                const j: Index = @bitCast(code[i..][0..n].*);
                try writer.print("{d} {s} {d}@{d}\n", .{ i, names.keys()[op], j.index, j.depth });
                i += n;
            },
            names.getIndex("new").?, names.getIndex("call").? => {
                try writer.print("{d} {s} {d}\n", .{ i, names.keys()[op], code[i] });
                i += 1;
            },
            names.getIndex("jmp").?, names.getIndex("jif").?, names.getIndex("ebb").?, names.getIndex("ret").? => {
                const offset = mem.readInt(usize, code[i..][0..N], native_endian);
                try writer.print("{d} {s} {d}\n", .{ i, names.keys()[op], offset });
                i += N;
            },
            else => {
                try writer.print("{d} {s}\n", .{ i, names.keys()[op] });
            },
        }
    }
}
