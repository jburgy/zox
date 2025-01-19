const std = @import("std");
const builtin = @import("builtin");
const debug = std.debug;
const assert = debug.assert;
const testing = std.testing;
const math = std.math;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const native_endian = builtin.cpu.arch.endian();
const evaluate = @import("evaluate.zig");
const EvaluationError = evaluate.EvaluationError;
const Value = evaluate.Value;

const Stack = std.ArrayListUnmanaged(Value);
const Instruction = fn (Allocator, *Stack, []const u8) EvaluationError!void;
const InstructionPointer = *const fn (Allocator, *Stack, []const u8) EvaluationError!void;

fn end(allocator: Allocator, stack: *Stack, program: []const u8) !void {
    _ = allocator;
    _ = stack;
    assert(program.len == 0);
}

fn str(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    const s = mem.sliceTo(program, 0);
    const n = s.len + 1; // skip past nil sentinel
    stack.appendAssumeCapacity(.{ .string = program[0..s.len] });
    try @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..] });
}

test str {
    var buffer = [_]Value{.{ .nil = {} }};
    var stack = Stack.initBuffer(buffer[0..]);
    const expected = "Hello, World!";
    var program = .{opcode("str")} ++ expected ++ .{ 0, opcode("end") };

    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    assert(stack.items.len == 1);
    try testing.expectEqualStrings(expected, stack.pop().string);
}

fn num(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    const n = @sizeOf(std.meta.FieldType(Value, .number));
    stack.appendAssumeCapacity(.{ .number = @bitCast(program[0..n].*) });
    try @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..] });
}

test num {
    var buffer = [_]Value{.{ .nil = {} }} ** 2;
    var stack = Stack.initBuffer(buffer[0..]);
    const n = @sizeOf(std.meta.FieldType(Value, .number));
    const program = .{opcode("num")} ++ .{0} ** n ++ .{opcode("num")} ++ .{0} ** (n - 2) ++ [_]u8{ 0xf8, 0x7f, opcode("end") };

    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    assert(stack.items.len == 2);
    assert(std.math.isNan(stack.pop().number));
    assert(stack.pop().number == 0.0);
}

fn pop(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    _ = stack.pop();
    try @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..] });
}

test pop {
    var buffer = [_]Value{.{ .nil = {} }};
    var stack = Stack.initBuffer(buffer[0..]);
    const program = [_]u8{ opcode("pop"), opcode("end") };

    stack.appendAssumeCapacity(.{ .bool = true });
    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    assert(stack.items.len == 0);
}

fn dup(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    stack.appendAssumeCapacity(stack.getLast());
    try @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..] });
}

test dup {
    var buffer = [_]Value{.{ .nil = {} }} ** 2;
    var stack = Stack.initBuffer(buffer[0..]);
    const program = [_]u8{ opcode("dup"), opcode("end") };

    stack.appendAssumeCapacity(.{ .bool = true });
    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    assert(stack.items.len == 2);
    assert(stack.pop().bool);
    assert(stack.pop().bool);
}

fn not(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    stack.appendAssumeCapacity(.{ .bool = !stack.pop().truthy() });
    try @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..] });
}

test not {
    var buffer = [_]Value{.{ .nil = {} }};
    var stack = Stack.initBuffer(buffer[0..]);
    const program = [_]u8{ opcode("not"), opcode("end") };

    stack.appendAssumeCapacity(.{ .bool = false });
    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    assert(stack.items.len == 1);
    assert(stack.pop().bool);
}

fn jmp(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    const m = @sizeOf(usize);
    const n = mem.readInt(usize, program[0..m], native_endian) + m;
    try @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..] });
}

test jmp {
    var stack = Stack.initBuffer(&[_]Value{});
    const program = .{opcode("jmp")} ++ [_]u8{0} ** @sizeOf(usize) ++ .{opcode("end")};
    try instructions[program[0]](testing.allocator, &stack, program[1..]);
}

fn jif(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    const m = @sizeOf(usize);
    const n = if (stack.pop().truthy()) m else mem.readInt(usize, program[0..m], native_endian) + m;
    try @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..] });
}

fn binary(comptime op: fn (allocator: Allocator, a: Value, b: Value) EvaluationError!Value) Instruction {
    return struct {
        fn wrap(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
            stack.appendAssumeCapacity(try op(allocator, stack.pop(), stack.pop()));
            try @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..] });
        }
    }.wrap;
}

fn add(allocator: Allocator, a: Value, b: Value) EvaluationError!Value {
    return switch (a) {
        .number => |lhs| switch (b) {
            .number => |rhs| .{ .number = lhs + rhs },
            else => error.OperandsMustBeNumbers,
        },
        .string => |lhs| switch (b) {
            .string => |rhs| blk: {
                const s = try allocator.alloc(u8, lhs.len + rhs.len);
                @memcpy(s[0..lhs.len], lhs);
                @memcpy(s[lhs.len..], rhs);
                break :blk .{ .string = s };
            },
            else => error.OperandsMustBeNumbers,
        },
        else => unreachable,
    };
}

test add {
    var buffer = [_]Value{.{ .number = 1.0 }} ** 2;
    var stack = Stack{ .items = &buffer, .capacity = buffer.len };
    var program = [_]u8{ opcode("add"), opcode("end") };

    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    assert(stack.items.len == 1);
    assert(stack.pop().number == 2.0);
}

fn sub(allocator: Allocator, a: Value, b: Value) EvaluationError!Value {
    _ = allocator;
    return switch (a) {
        .number => |lhs| switch (b) {
            .number => |rhs| .{ .number = lhs - rhs },
            else => error.OperandsMustBeNumbers,
        },
        else => error.OperandsMustBeNumbers,
    };
}

fn mul(allocator: Allocator, a: Value, b: Value) EvaluationError!Value {
    _ = allocator;
    return switch (a) {
        .number => |lhs| switch (b) {
            .number => |rhs| .{ .number = lhs * rhs },
            else => error.OperandsMustBeNumbers,
        },
        else => error.OperandsMustBeNumbers,
    };
}

fn div(allocator: Allocator, a: Value, b: Value) EvaluationError!Value {
    _ = allocator;
    return switch (a) {
        .number => |lhs| switch (b) {
            .number => |rhs| .{ .number = lhs / rhs },
            else => error.OperandsMustBeNumbers,
        },
        else => error.OperandsMustBeNumbers,
    };
}

fn compare(comptime op: math.CompareOperator) Instruction {
    return struct {
        fn wrap(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
            stack.appendAssumeCapacity(.{ .bool = try switch (stack.pop()) {
                .number => |lhs| switch (stack.pop()) {
                    .number => |rhs| math.compare(lhs, op, rhs),
                    else => error.OperandsMustBeNumbers,
                },
                .string => |lhs| switch (stack.pop()) {
                    .string => |rhs| mem.order(u8, lhs, rhs).compare(op),
                    else => error.OperandsMustBeStrings,
                },
                else => blk: {
                    _ = stack.pop();
                    break :blk error.OperandsMustBeNumbers;
                },
            } });
            try @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..] });
        }
    }.wrap;
}

fn compareTester(a: Value, op: u8, b: Value) !bool {
    var buffer = [_]Value{ b, a };
    var stack = Stack{ .items = &buffer, .capacity = buffer.len };
    const program = [_]u8{ op, opcode("end") };

    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    assert(stack.items.len == 1);
    return stack.pop().bool;
}

test compare {
    assert(try compareTester(.{ .number = 2.0 }, opcode("lt"), .{ .number = 3.0 }));
    assert(try compareTester(.{ .number = 2.0 }, opcode("lte"), .{ .number = 3.0 }));
    assert(!try compareTester(.{ .number = 2.0 }, opcode("gt"), .{ .number = 3.0 }));
    assert(!try compareTester(.{ .number = 2.0 }, opcode("gte"), .{ .number = 3.0 }));

    assert(!try compareTester(.{ .string = "foo" }, opcode("lt"), .{ .string = "bar" }));
    assert(!try compareTester(.{ .string = "foo" }, opcode("lte"), .{ .string = "bar" }));
    assert(try compareTester(.{ .string = "foo" }, opcode("gt"), .{ .string = "bar" }));
    assert(try compareTester(.{ .string = "foo" }, opcode("gte"), .{ .string = "bar" }));
}

fn equal(ok: bool) Instruction {
    return struct {
        fn wrap(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
            stack.appendAssumeCapacity(.{ .bool = switch (stack.pop()) {
                .bool => |lhs| switch (stack.pop()) {
                    .bool => |rhs| (lhs == rhs) == ok,
                    else => !ok,
                },
                .number => |lhs| switch (stack.pop()) {
                    .number => |rhs| (lhs == rhs) == ok,
                    else => !ok,
                },
                .string => |lhs| switch (stack.pop()) {
                    .string => |rhs| mem.eql(u8, lhs, rhs) == ok,
                    else => !ok,
                },
                else => blk: {
                    _ = stack.pop();
                    break :blk !ok;
                },
            } });
            try @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..] });
        }
    }.wrap;
}

test equal {
    assert(!try compareTester(.{ .bool = true }, opcode("eql"), .{ .bool = false }));
    assert(try compareTester(.{ .bool = true }, opcode("neq"), .{ .bool = false }));
    assert(!try compareTester(.{ .number = 2.0 }, opcode("eql"), .{ .number = 3.0 }));
    assert(try compareTester(.{ .number = 2.0 }, opcode("neq"), .{ .number = 3.0 }));
    assert(!try compareTester(.{ .string = "foo" }, opcode("eql"), .{ .string = "bar" }));
    assert(try compareTester(.{ .string = "foo" }, opcode("neq"), .{ .string = "bar" }));

    assert(!try compareTester(.{ .bool = true }, opcode("eql"), .{ .nil = {} }));
    assert(!try compareTester(.{ .bool = true }, opcode("eql"), .{ .number = 3.0 }));
    assert(!try compareTester(.{ .bool = true }, opcode("eql"), .{ .string = "bar" }));
    assert(try compareTester(.{ .bool = true }, opcode("neq"), .{ .nil = {} }));
    assert(try compareTester(.{ .bool = true }, opcode("neq"), .{ .number = 3.0 }));
    assert(try compareTester(.{ .bool = true }, opcode("neq"), .{ .string = "bar" }));
}

const names = std.StaticStringMap(InstructionPointer).initComptime(.{
    .{ "end", &end },
    .{ "num", &num },
    .{ "str", &str },
    .{ "pop", &pop },
    .{ "dup", &dup },
    .{ "not", &not },
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
