const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const math = std.math;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const native_endian = builtin.cpu.arch.endian();
const evaluate = @import("evaluate.zig");
const EvaluationError = evaluate.EvaluationError;
pub const Value = evaluate.Value;

pub const Stack = std.ArrayListUnmanaged(Value);
const Instruction = fn (Allocator, *Stack, []const u8) EvaluationError!void;
const InstructionPointer = *const fn (Allocator, *Stack, []const u8) EvaluationError!void;

fn end(allocator: Allocator, stack: *Stack, program: []const u8) !void {
    _ = allocator;
    _ = stack;
    std.debug.assert(program.len == 0);
}

fn str(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    const s = mem.sliceTo(program, 0);
    const n = s.len + 1; // skip past nil sentinel
    stack.appendAssumeCapacity(.{ .string = program[0..s.len] });
    try @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..] });
}

fn test_stack(n: comptime_int) Stack {
    return Stack.initBuffer(@constCast(&[_]Value{.{ .nil = {} }} ** n));
}

test str {
    var stack = test_stack(1);
    const expected = "Hello, World!";
    const program = .{opcode("str")} ++ expected ++ .{ 0, opcode("end") };

    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    try testing.expectEqual(1, stack.items.len);
    try testing.expectEqualStrings(expected, stack.pop().string);
}

fn num(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    const n = @sizeOf(std.meta.FieldType(Value, .number));
    stack.appendAssumeCapacity(.{ .number = @bitCast(program[0..n].*) });
    try @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..] });
}

test num {
    var stack = test_stack(2);
    const t = std.meta.FieldType(Value, .number);
    const program = .{opcode("num")} ++ mem.toBytes(@as(t, 0.0)) ++ .{opcode("num")} ++ mem.toBytes(math.nan(t)) ++ .{opcode("end")};

    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    try testing.expectEqual(2, stack.items.len);
    try testing.expect(math.isNan(stack.pop().number));
    try testing.expectEqual(0.0, stack.pop().number);
}

fn pop(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    _ = stack.pop();
    try @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..] });
}

test pop {
    var stack = test_stack(1);
    const program = [_]u8{ opcode("pop"), opcode("end") };

    stack.appendAssumeCapacity(.{ .bool = true });
    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    try testing.expectEqual(0, stack.items.len);
}

fn dup(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    stack.appendAssumeCapacity(stack.getLast());
    try @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..] });
}

test dup {
    var stack = test_stack(2);
    const program = [_]u8{ opcode("dup"), opcode("end") };

    stack.appendAssumeCapacity(.{ .bool = true });
    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    try testing.expectEqual(2, stack.items.len);
    try testing.expect(stack.pop().bool);
    try testing.expect(stack.pop().bool);
}

fn not(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    stack.appendAssumeCapacity(.{ .bool = !stack.pop().truthy() });
    try @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..] });
}

test not {
    var stack = test_stack(1);
    const program = [_]u8{ opcode("not"), opcode("end") };

    stack.appendAssumeCapacity(.{ .bool = false });
    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    try testing.expectEqual(1, stack.items.len);
    try testing.expect(stack.pop().bool);
}

fn jmp(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    const m = @sizeOf(usize);
    const n = mem.readInt(usize, program[0..m], native_endian) + m;
    try @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..] });
}

test jmp {
    var stack = test_stack(0);
    const program = .{opcode("jmp")} ++ mem.toBytes(@as(usize, 0)) ++ .{opcode("end")};
    try instructions[program[0]](testing.allocator, &stack, program[1..]);
}

fn jif(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
    const m = @sizeOf(usize);
    const n = if (stack.pop().truthy()) m else mem.readInt(usize, program[0..m], native_endian) + m;
    try @call(.always_tail, instructions[program[n]], .{ allocator, stack, program[n + 1 ..] });
}

test jif {
    var stack = test_stack(1);
    const program = .{opcode("jif")} ++ mem.toBytes(@as(usize, 0)) ++ .{opcode("end")};

    stack.appendAssumeCapacity(.{ .bool = false });
    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    try testing.expectEqual(0, stack.items.len);
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
    var stack = test_stack(2);
    const program = [_]u8{ opcode("add"), opcode("end") };

    for (0..2) |_| stack.appendAssumeCapacity(.{ .number = 1.0 });
    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    try testing.expectEqual(1, stack.items.len);
    try testing.expectEqual(2.0, stack.pop().number);
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
            const b = stack.pop();
            const a = stack.pop();
            stack.appendAssumeCapacity(.{ .bool = try switch (a) {
                .number => |lhs| switch (b) {
                    .number => |rhs| math.compare(lhs, op, rhs),
                    else => error.OperandsMustBeNumbers,
                },
                .string => |lhs| switch (b) {
                    .string => |rhs| mem.order(u8, lhs, rhs).compare(op),
                    else => error.OperandsMustBeStrings,
                },
                else => error.OperandsMustBeNumbers,
            } });
            try @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..] });
        }
    }.wrap;
}

fn compareTester(a: Value, op: u8, b: Value) !bool {
    var stack = test_stack(2);
    const program = [_]u8{ op, opcode("end") };

    stack.appendAssumeCapacity(a);
    stack.appendAssumeCapacity(b);
    try instructions[program[0]](testing.allocator, &stack, program[1..]);
    try testing.expectEqual(1, stack.items.len);
    return stack.pop().bool;
}

test compare {
    try testing.expect(try compareTester(.{ .number = 2.0 }, opcode("lt"), .{ .number = 3.0 }));
    try testing.expect(try compareTester(.{ .number = 2.0 }, opcode("lte"), .{ .number = 3.0 }));
    try testing.expect(!try compareTester(.{ .number = 2.0 }, opcode("gt"), .{ .number = 3.0 }));
    try testing.expect(!try compareTester(.{ .number = 2.0 }, opcode("gte"), .{ .number = 3.0 }));

    try testing.expect(!try compareTester(.{ .string = "foo" }, opcode("lt"), .{ .string = "bar" }));
    try testing.expect(!try compareTester(.{ .string = "foo" }, opcode("lte"), .{ .string = "bar" }));
    try testing.expect(try compareTester(.{ .string = "foo" }, opcode("gt"), .{ .string = "bar" }));
    try testing.expect(try compareTester(.{ .string = "foo" }, opcode("gte"), .{ .string = "bar" }));
}

fn equal(ok: bool) Instruction {
    return struct {
        fn wrap(allocator: Allocator, stack: *Stack, program: []const u8) EvaluationError!void {
            const b = stack.pop();
            const a = stack.pop();
            stack.appendAssumeCapacity(.{ .bool = switch (a) {
                .bool => |lhs| switch (b) {
                    .bool => |rhs| (lhs == rhs) == ok,
                    else => !ok,
                },
                .number => |lhs| switch (b) {
                    .number => |rhs| (lhs == rhs) == ok,
                    else => !ok,
                },
                .string => |lhs| switch (b) {
                    .string => |rhs| mem.eql(u8, lhs, rhs) == ok,
                    else => !ok,
                },
                else => !ok,
            } });
            try @call(.always_tail, instructions[program[0]], .{ allocator, stack, program[1..] });
        }
    }.wrap;
}

test equal {
    try testing.expect(!try compareTester(.{ .bool = true }, opcode("eql"), .{ .bool = false }));
    try testing.expect(try compareTester(.{ .bool = true }, opcode("neq"), .{ .bool = false }));
    try testing.expect(!try compareTester(.{ .number = 2.0 }, opcode("eql"), .{ .number = 3.0 }));
    try testing.expect(try compareTester(.{ .number = 2.0 }, opcode("neq"), .{ .number = 3.0 }));
    try testing.expect(!try compareTester(.{ .string = "foo" }, opcode("eql"), .{ .string = "bar" }));
    try testing.expect(try compareTester(.{ .string = "foo" }, opcode("neq"), .{ .string = "bar" }));

    try testing.expect(!try compareTester(.{ .bool = true }, opcode("eql"), .{ .nil = {} }));
    try testing.expect(!try compareTester(.{ .bool = true }, opcode("eql"), .{ .number = 3.0 }));
    try testing.expect(!try compareTester(.{ .bool = true }, opcode("eql"), .{ .string = "bar" }));
    try testing.expect(try compareTester(.{ .bool = true }, opcode("neq"), .{ .nil = {} }));
    try testing.expect(try compareTester(.{ .bool = true }, opcode("neq"), .{ .number = 3.0 }));
    try testing.expect(try compareTester(.{ .bool = true }, opcode("neq"), .{ .string = "bar" }));
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

pub fn run(allocator: Allocator, stack: *Stack, program: []const u8) !void {
    try instructions[program[0]](allocator, stack, program[1..]);
}
