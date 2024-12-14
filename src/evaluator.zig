const std = @import("std");
const Node = @import("parser.zig").Node;

const stdout = std.io.getStdOut().writer();

const EvaluationError = error{
    OperandMustBeANumber,
    OperandsMustBeNumbers,
    UndefinedVariable,
};

const ValueType = enum {
    nil,
    bool,
    string,
    number,
    native,
    function,
};

fn wrap(comptime function: anytype) Value {
    const Function = @TypeOf(function);
    const Args = std.meta.ArgsTuple(Function);

    const function_info = @typeInfo(Function).Fn;
    const params = function_info.params;
    const return_info = @typeInfo(function_info.return_type.?);

    return .{ .native = struct {
        fn call(args: []const Value) Value {
            const argt: Args = undefined;
            inline for (params, args, argt) |param, s, *t| {
                t.* = switch (param.type) {
                    .Int, .Float => s.number,
                    else => unreachable,
                };
            }
            const result = @call(.auto, function, argt);
            return switch (return_info) {
                .Int => .{ .number = @floatFromInt(result) },
                .Float => .{ .number = result },
                else => unreachable,
            };
        }
    }.call };
}

const Value = union(ValueType) {
    nil: void,
    bool: bool,
    string: []const u8,
    number: f64,
    native: *const fn ([]const Value) Value,
    function: []const *const Node,

    pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try switch (value) {
            .nil => writer.print("nil", .{}),
            .bool => |b| writer.print("{any}", .{b}),
            .string => |s| writer.print("{s}", .{s}),
            .number => |d| writer.print("{d}", .{d}),
            .native => |f| writer.print("{any}", .{f}),
            .function => |f| writer.print("<fn {any}>", .{f[0].token}),
        };
    }

    pub fn truthy(value: @This()) bool {
        return switch (value) {
            .nil => false,
            .bool => |b| b,
            .string, .native, .function => true,
            .number => |x| x != 0.0,
        };
    }
};

const ValueMap = std.StringHashMap(Value);
const ValueMaps = std.SinglyLinkedList(ValueMap);

pub const Evaluator = struct {
    allocator: std.mem.Allocator,
    source: []const u8,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) @This() {
        return .{ .allocator = allocator, .source = source };
    }

    pub fn createEnv(self: @This()) !ValueMaps {
        var globals = try self.allocator.create(ValueMaps.Node);
        globals.data = ValueMap.init(self.allocator);
        try globals.data.put("clock", wrap(std.time.timestamp));
        var env = ValueMaps{};
        env.prepend(globals);
        return env;
    }

    pub fn evaluate(self: @This(), node: *const Node, env: *ValueMaps) !Value {
        const slice = node.token.source(self.source);
        return switch (node.token.tag) {
            .FALSE => .{ .bool = false },
            .NIL => .{ .nil = void{} },
            .TRUE => .{ .bool = true },
            .STRING => if (slice[slice.len - 1] == '"') .{ .string = slice[1 .. slice.len - 1] } else error.UnexpectedToken,
            .NUMBER => .{ .number = try std.fmt.parseFloat(f64, slice) },
            .LEFT_PAREN => try self.evaluate(node.args[0], env),
            .MINUS => if (node.args.len == 1) switch (try self.evaluate(node.args[0], env)) {
                .number => |lhs| .{ .number = -lhs },
                else => error.OperandMustBeANumber,
            } else switch (try self.evaluate(node.args[0], env)) {
                .number => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .number => |rhs| .{ .number = lhs - rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .PLUS => switch (try self.evaluate(node.args[0], env)) {
                .number => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .number => |rhs| .{ .number = lhs + rhs },
                    else => error.OperandsMustBeNumbers,
                },
                .string => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .string => |rhs| blk: {
                        const str = try self.allocator.alloc(u8, lhs.len + rhs.len);
                        @memcpy(str[0..lhs.len], lhs);
                        @memcpy(str[lhs.len..], rhs);
                        break :blk .{ .string = str };
                    },
                    else => error.OperandsMustBeNumbers,
                },
                else => unreachable,
            },
            .BANG => .{ .bool = !(try self.evaluate(node.args[0], env)).truthy() },
            .STAR => switch (try self.evaluate(node.args[0], env)) {
                .number => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .number => |rhs| .{ .number = lhs * rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .SLASH => switch (try self.evaluate(node.args[0], env)) {
                .number => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .number => |rhs| .{ .number = lhs / rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .LESS => switch (try self.evaluate(node.args[0], env)) {
                .number => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .number => |rhs| .{ .bool = lhs < rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .LESS_EQUAL => switch (try self.evaluate(node.args[0], env)) {
                .number => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .number => |rhs| .{ .bool = lhs <= rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .GREATER => switch (try self.evaluate(node.args[0], env)) {
                .number => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .number => |rhs| .{ .bool = lhs > rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .GREATER_EQUAL => switch (try self.evaluate(node.args[0], env)) {
                .number => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .number => |rhs| .{ .bool = lhs >= rhs },
                    else => error.OperandsMustBeNumbers,
                },
                else => error.OperandsMustBeNumbers,
            },
            .EQUAL_EQUAL => .{ .bool = switch (try self.evaluate(node.args[0], env)) {
                .number => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .number => |rhs| lhs == rhs,
                    else => false,
                },
                .string => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .string => |rhs| std.mem.eql(u8, lhs, rhs),
                    else => false,
                },
                .bool => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .bool => |rhs| lhs == rhs,
                    else => false,
                },
                else => false,
            } },
            .BANG_EQUAL => .{ .bool = switch (try self.evaluate(node.args[0], env)) {
                .number => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .number => |rhs| lhs != rhs,
                    else => true,
                },
                .string => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .string => |rhs| std.mem.eql(u8, lhs, rhs) == false,
                    else => true,
                },
                .bool => |lhs| switch (try self.evaluate(node.args[1], env)) {
                    .bool => |rhs| lhs != rhs,
                    else => true,
                },
                else => true,
            } },
            .PRINT => .{ .nil = {
                const value = try self.evaluate(node.args[0], env);
                try switch (value) {
                    .function => |args| stdout.print("<fn {s}>", .{args[0].token.source(self.source)}),
                    else => stdout.print("{any}\n", .{value}),
                };
            } },
            .SEMICOLON => .{ .nil = for (node.args) |arg| {
                _ = try self.evaluate(arg, env);
            } },
            .VAR => .{ .nil = {
                if (env.first) |n|
                    try n.data.put(
                        node.args[0].token.source(self.source),
                        switch (node.args.len) {
                            2 => try self.evaluate(node.args[1], env),
                            else => .{ .nil = {} },
                        },
                    )
                else
                    unreachable;
            } },
            .IDENTIFIER => blk: {
                var it = env.first;
                while (it) |n| : (it = n.next) {
                    const scope = n.data;
                    if (scope.get(slice)) |value| {
                        break :blk value;
                    } else continue;
                }
                std.debug.print(
                    "Undefined variable '{s}'.\n[Line {d}]",
                    .{ node.token.source(self.source), node.token.line(self.source) },
                );
                break :blk error.UndefinedVariable;
            },
            .EQUAL => blk: {
                const lhs = node.args[0].token;
                const key = lhs.source(self.source);
                var it = env.first;
                while (it) |n| : (it = n.next) {
                    var scope = n.data;
                    if (scope.get(key)) |_| {
                        const val = try self.evaluate(node.args[1], env);
                        try scope.put(key, val);
                        break :blk val;
                    } else continue;
                }
                std.debug.print(
                    "Undefined variable '{s}'.\n[Line {d}]",
                    .{ key, lhs.line(self.source) },
                );
                break :blk error.UndefinedVariable;
            },
            .LEFT_BRACE => .{ .nil = {
                var scope = ValueMaps.Node{ .data = ValueMap.init(self.allocator) };
                defer scope.data.deinit();
                env.prepend(&scope);
                _ = try self.evaluate(node.args[0], env);
                _ = env.popFirst();
            } },
            .IF => .{ .nil = {
                const cond = try self.evaluate(node.args[0], env);
                if (cond.truthy()) {
                    _ = try self.evaluate(node.args[1], env);
                } else if (node.args.len > 2) {
                    _ = try self.evaluate(node.args[2], env);
                }
            } },
            .OR => blk: {
                const lhs = try self.evaluate(node.args[0], env);
                break :blk if (lhs.truthy()) lhs else try self.evaluate(node.args[1], env);
            },
            .AND => blk: {
                const lhs = try self.evaluate(node.args[0], env);
                break :blk if (!lhs.truthy()) lhs else try self.evaluate(node.args[1], env);
            },
            .WHILE => .{ .nil = {
                while ((try self.evaluate(node.args[0], env)).truthy()) {
                    _ = try self.evaluate(node.args[1], env);
                }
            } },
            .FOR => .{ .nil = {
                _ = try self.evaluate(node.args[0], env);
                while ((try self.evaluate(node.args[1], env)).truthy()) {
                    _ = try self.evaluate(node.args[2], env);
                    if (node.args.len > 3)
                        _ = try self.evaluate(node.args[3], env);
                }
            } },
            .RIGHT_PAREN => blk: {
                const func = try self.evaluate(node.args[0], env);
                const m = node.args.len - 1;
                const args: []Value = try self.allocator.alloc(Value, m);
                defer self.allocator.free(args);
                if (m > 1) {
                    for (node.args[1..m], args) |n, *value|
                        value.* = try self.evaluate(n, env);
                }
                switch (func) {
                    .native => |f| break :blk f(args),
                    .function => |nodes| {
                        var scope = ValueMaps.Node{ .data = ValueMap.init(self.allocator) };
                        defer scope.data.deinit();
                        env.prepend(&scope);
                        if (m > 1)
                            for (nodes[1..m], args) |param, arg| try scope.data.put(param.token.source(self.source), arg);
                        break :blk self.evaluate(nodes[m + 1], env);
                    },
                    else => unreachable,
                }
            },
            .FUN => .{ .nil = {
                if (env.first) |scope|
                    try scope.data.put(
                        node.args[0].token.source(self.source),
                        .{ .function = node.args },
                    )
                else
                    unreachable;
            } },
            else => unreachable,
        };
    }
};
