const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const math = std.math;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const native_endian = builtin.cpu.arch.endian();
const evaluate = @import("evaluate.zig");
const value = @import("value.zig");

pub const Stack = std.ArrayListUnmanaged(f64);
const Instruction = fn (Allocator, *Stack, []const u8, *Values) void;
const InstructionPointer = *const fn (Allocator, *Stack, []const u8, *Values) void;

pub const Values = std.SinglyLinkedList([]f64);
pub const Index = packed struct(u32) { depth: u8, index: u24 };

fn test_stack(n: comptime_int) Stack {
    return Stack.initBuffer(@constCast(&[_]f64{math.nan(f64)} ** n));
}

fn end(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) void {
    _ = allocator;
    _ = stack;
    _ = values;
    std.debug.assert(program.len == 0);
}

fn str(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) void {
    const s = mem.sliceTo(program, 0);
    const n = s.len + 1; // skip past nil sentinel
    stack.appendAssumeCapacity(value.box(s));
    @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..], values });
}

test str {
    var stack = test_stack(1);
    const expected = "Hello, World!";
    const program = .{opcode("str")} ++ expected ++ .{ 0, opcode("end") };
    var values = Values{};

    instructions[program[0]](testing.allocator, &stack, program[1..], &values);
    try testing.expectEqual(1, stack.items.len);
    try testing.expectEqualStrings(expected, value.unbox(stack.pop()).string);
}

fn num(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) void {
    const n = @sizeOf(f64);
    stack.appendAssumeCapacity(@bitCast(program[0..n].*));
    @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..], values });
}

test num {
    var stack = test_stack(2);
    const program = .{opcode("num")} ++ mem.toBytes(@as(f64, 0.0)) ++ .{opcode("num")} ++ mem.toBytes(math.nan(f64)) ++ .{opcode("end")};
    var values = Values{};

    instructions[program[0]](testing.allocator, &stack, program[1..], &values);
    try testing.expectEqual(2, stack.items.len);
    try testing.expect(math.isNan(stack.pop()));
    try testing.expectEqual(0.0, stack.pop());
}

fn pop(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) void {
    _ = stack.pop();
    @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..], values });
}

test pop {
    var stack = test_stack(1);
    const program = [_]u8{ opcode("pop"), opcode("end") };
    var values = Values{};

    stack.appendAssumeCapacity(value.box(true));
    instructions[program[0]](testing.allocator, &stack, program[1..], &values);
    try testing.expectEqual(0, stack.items.len);
}

fn dup(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) void {
    stack.appendAssumeCapacity(stack.getLast());
    @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..], values });
}

test dup {
    var stack = test_stack(2);
    const program = [_]u8{ opcode("dup"), opcode("end") };
    var values = Values{};

    stack.appendAssumeCapacity(1.0);
    instructions[program[0]](testing.allocator, &stack, program[1..], &values);
    try testing.expectEqual(2, stack.items.len);
    try testing.expectEqual(1.0, stack.pop());
    try testing.expectEqual(1.0, stack.pop());
}

fn not(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) void {
    stack.appendAssumeCapacity(value.box(!value.truthy(stack.pop())));
    @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..], values });
}

test not {
    var stack = test_stack(1);
    const program = [_]u8{ opcode("not"), opcode("end") };
    var values = Values{};

    stack.appendAssumeCapacity(value.box(false));
    instructions[program[0]](testing.allocator, &stack, program[1..], &values);
    try testing.expectEqual(1, stack.items.len);
    try testing.expect(value.truthy(stack.pop()));
}

fn get(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) void {
    const n = @sizeOf(Index);
    const i: Index = @bitCast(program[0..n].*);

    var p = values.first;
    for (0..i.depth) |_| p = p.?.next;
    stack.appendAssumeCapacity(p.?.data[i.index]);
    @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..], values });
}

test get {
    var stack = test_stack(1);
    const program = .{opcode("get")} ++ mem.toBytes(Index{ .depth = 0, .index = 0 }) ++ .{opcode("end")};
    var values = Values{};
    var locals = [_]f64{value.box(@as([*:0]const u8, "Hello, world!"))};
    var scope = Values.Node{ .data = locals[0..] };

    values.prepend(&scope);
    instructions[program[0]](testing.allocator, &stack, program[1..], &values);
    try testing.expectEqual(1, stack.items.len);
    try testing.expectEqualStrings("Hello, world!", value.unbox(stack.pop()).string);
}

fn set(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) void {
    const n = @sizeOf(Index);
    const i: Index = @bitCast(program[0..n].*);

    var p = values.first;
    for (0..i.depth) |_| p = p.?.next;
    p.?.data[i.index] = stack.pop();
    @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..], values });
}

test set {
    var stack = test_stack(1);
    const program = .{opcode("set")} ++ mem.toBytes(Index{ .depth = 0, .index = 0 }) ++ .{opcode("end")};
    var values = Values{};
    var locals = [_]f64{value.box({})};
    var scope = Values.Node{ .data = locals[0..] };

    values.prepend(&scope);
    stack.appendAssumeCapacity(value.box(@as([*:0]const u8, "Hello, world!")));
    instructions[program[0]](testing.allocator, &stack, program[1..], &values);
    try testing.expectEqual(0, stack.items.len);
    try testing.expectEqualStrings("Hello, world!", value.unbox(locals[0]).string);
}

fn jmp(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) void {
    const m = @sizeOf(usize);
    const n = mem.readInt(usize, program[0..m], native_endian) + m;
    @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..], values });
}

test jmp {
    var stack = test_stack(0);
    const program = .{opcode("jmp")} ++ mem.toBytes(@as(usize, 0)) ++ .{opcode("end")};
    var values = Values{};
    instructions[program[0]](testing.allocator, &stack, program[1..], &values);
}

fn jif(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) void {
    const m = @sizeOf(usize);
    const n = if (value.truthy(stack.pop())) m else mem.readInt(usize, program[0..m], native_endian) + m;
    @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..], values });
}

test jif {
    var stack = test_stack(1);
    const program = .{opcode("jif")} ++ mem.toBytes(@as(usize, 0)) ++ .{opcode("end")};
    var values = Values{};

    stack.appendAssumeCapacity(value.box(false));
    instructions[program[0]](testing.allocator, &stack, program[1..], &values);
    try testing.expectEqual(0, stack.items.len);
}

fn binary(comptime op: fn (a: f64, b: f64) f64) Instruction {
    return struct {
        fn wrap(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) void {
            stack.appendAssumeCapacity(op(stack.pop(), stack.pop()));
            @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..], values });
        }
    }.wrap;
}

fn add(a: f64, b: f64) f64 {
    return a + b;
}

test add {
    var stack = test_stack(2);
    const program = [_]u8{ opcode("add"), opcode("end") };
    var values = Values{};

    for (0..2) |_| stack.appendAssumeCapacity(1.0);
    instructions[program[0]](testing.allocator, &stack, program[1..], &values);
    try testing.expectEqual(1, stack.items.len);
    try testing.expectEqual(2.0, stack.pop());
}

fn sub(a: f64, b: f64) f64 {
    return a + b;
}

fn mul(a: f64, b: f64) f64 {
    return a - b;
}

fn div(a: f64, b: f64) f64 {
    return a / b;
}

fn compare(comptime op: math.CompareOperator) Instruction {
    return struct {
        fn wrap(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) void {
            const b = stack.pop();
            const a = stack.pop();
            stack.appendAssumeCapacity(value.box(switch (value.tag(a)) {
                value.String => switch (value.tag(b)) {
                    value.String => mem.order(u8, value.unbox(a).string, value.unbox(b).string).compare(op),
                    else => false,
                },
                else => math.compare(a, op, b),
            }));
            @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..], values });
        }
    }.wrap;
}

fn compareTester(a: f64, op: u8, b: f64) !bool {
    var stack = test_stack(2);
    const program = [_]u8{ op, opcode("end") };
    var values = Values{};

    stack.appendAssumeCapacity(a);
    stack.appendAssumeCapacity(b);
    instructions[program[0]](testing.allocator, &stack, program[1..], &values);
    try testing.expectEqual(1, stack.items.len);
    return value.truthy(stack.pop());
}

test compare {
    const foo = value.box(@as([*:0]const u8, "foo"));
    const bar = value.box(@as([*:0]const u8, "bar"));

    try testing.expect(try compareTester(2.0, opcode("lt"), 3.0));
    try testing.expect(try compareTester(2.0, opcode("lte"), 3.0));
    try testing.expect(!try compareTester(2.0, opcode("gt"), 3.0));
    try testing.expect(!try compareTester(2.0, opcode("gte"), 3.0));

    try testing.expect(!try compareTester(foo, opcode("lt"), bar));
    try testing.expect(!try compareTester(foo, opcode("lte"), bar));
    try testing.expect(try compareTester(foo, opcode("gt"), bar));
    try testing.expect(try compareTester(foo, opcode("gte"), bar));
}

fn equal(ok: bool) Instruction {
    return struct {
        fn wrap(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) void {
            const b = stack.pop();
            const a = stack.pop();
            stack.appendAssumeCapacity(value.box(switch (value.tag(a)) {
                value.String => switch (value.tag(b)) {
                    value.String => mem.eql(u8, value.unbox(a).string, value.unbox(b).string) == ok,
                    else => !ok,
                },
                else => (a == b) == ok,
            }));
            @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..], values });
        }
    }.wrap;
}

test equal {
    const nil = value.box({});
    const true_ = value.box(true);
    const false_ = value.box(false);
    const foo = value.box(@as([*:0]const u8, "foo"));
    const bar = value.box(@as([*:0]const u8, "bar"));

    try testing.expect(!try compareTester(true_, opcode("eql"), false_));
    try testing.expect(try compareTester(true_, opcode("neq"), false_));
    try testing.expect(!try compareTester(2.0, opcode("eql"), 3.0));
    try testing.expect(try compareTester(2.0, opcode("neq"), 3.0));
    try testing.expect(!try compareTester(foo, opcode("eql"), bar));
    try testing.expect(try compareTester(foo, opcode("neq"), bar));

    try testing.expect(!try compareTester(true_, opcode("eql"), nil));
    try testing.expect(!try compareTester(true_, opcode("eql"), 3.0));
    try testing.expect(!try compareTester(true_, opcode("eql"), bar));
    try testing.expect(try compareTester(true_, opcode("neq"), nil));
    try testing.expect(try compareTester(true_, opcode("neq"), 3.0));
    try testing.expect(try compareTester(true_, opcode("neq"), bar));
}

const names = std.StaticStringMap(InstructionPointer).initComptime(.{
    .{ "end", &end },
    .{ "num", &num },
    .{ "str", &str },
    .{ "pop", &pop },
    .{ "dup", &dup },
    .{ "not", &not },
    .{ "get", &get },
    .{ "set", &set },
    .{ "jmp", &jmp },
    .{ "jif", &jif },
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

pub fn opcode(comptime name: [:0]const u8) u8 {
    return @truncate(names.getIndex(name) orelse 0);
}

pub fn run(allocator: Allocator, stack: *Stack, program: []const u8, values: *Values) !void {
    instructions[program[0]](allocator, stack, program[1..], values);
}
