const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;
const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const parse = @import("parse.zig");

pub const EvaluationError = error{
    OutOfMemory,
    InvalidCharacter,
    OperandMustBeANumber,
    OperandsMustBeNumbers,
    OperandsMustBeStrings,
    UndefinedVariable,
    EarlyExit,
    NotAFunction,
    WrongArgCount,
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

pub const Value = union(ValueType) {
    nil: void,
    bool: bool,
    string: []const u8,
    number: f64,
    native: *const fn ([]const Value) Value,
    function: struct { nodes: []const parse.Node, first: *ValueMaps.Node },

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

pub fn Evaluator(comptime WriterType: anytype) type {
    return struct {
        allocator: Allocator,
        source: []const u8,
        tokens: []const Token,
        nodes: []const parse.Node,
        writer: WriterType,

        const Self = @This();

        pub fn init(
            allocator: Allocator,
            source: []const u8,
            tokens: []const Token,
            nodes: []const parse.Node,
            writer: WriterType,
        ) Self {
            return .{
                .allocator = allocator,
                .source = source,
                .tokens = tokens,
                .nodes = nodes,
                .writer = writer,
            };
        }

        pub fn createEnv(self: Self) !ValueMaps {
            var globals = try self.createScope();
            try globals.data.put("clock", wrap(std.time.timestamp));
            var env = ValueMaps{};
            env.prepend(globals);
            return env;
        }

        fn createScope(self: Self) !*ValueMaps.Node {
            var node = try self.allocator.create(ValueMaps.Node);
            node.data = ValueMap.init(self.allocator);
            return node;
        }

        inline fn exists(list: ValueMaps, node: *ValueMaps.Node) bool {
            var it = list.first;
            while (it) |n| : (it = n.next) if (n == node) return true;
            return false;
        }

        pub fn destroyScope(self: Self, node: *ValueMaps.Node) void {
            var scope = node.data;
            if (if (scope.fetchRemove("")) |kv| switch (kv.value) {
                .function => |f| f.first == node,
                else => false,
            } else false) return;
            var it = scope.valueIterator();
            // Multiple functions might have closed over the same scope so we must be careful
            // to only destroy them once.  Naive implementation keeps track of them in a list.
            var scopes = ValueMaps{};
            while (it.next()) |val| {
                switch (val.*) {
                    .string => |s| {
                        const a = @intFromPtr(self.source.ptr);
                        const b = @intFromPtr(s.ptr);
                        if (!(a < b and b < a + self.source.len)) self.allocator.free(s);
                    },
                    .function => |f| {
                        if (!(f.first == node or exists(scopes, f.first)))
                            scopes.prepend(f.first);
                    },
                    else => {},
                }
            }
            while (scopes.popFirst()) |n| self.destroyScope(n);
            scope.clearAndFree();
            self.allocator.destroy(node);
        }

        pub fn evaluate(self: Self, node: usize, env: *ValueMaps) EvaluationError!Value {
            const nodes = self.nodes;
            const token = self.tokens[nodes[node].head.token];
            const slice = token.src;
            return switch (token.tag) {
                .FALSE => .{ .bool = false },
                .NIL => .{ .nil = void{} },
                .TRUE => .{ .bool = true },
                .STRING => .{ .string = slice[1 .. slice.len - 1] },
                .NUMBER => .{ .number = try std.fmt.parseFloat(f64, slice) },
                .LEFT_PAREN => try self.evaluate(nodes[node + 1].node, env),
                .MINUS => if (nodes[node].head.count == 1) switch (try self.evaluate(nodes[node + 1].node, env)) {
                    .number => |lhs| .{ .number = -lhs },
                    else => error.OperandMustBeANumber,
                } else switch (try self.evaluate(nodes[node + 1].node, env)) {
                    .number => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .number => |rhs| .{ .number = lhs - rhs },
                        else => error.OperandsMustBeNumbers,
                    },
                    else => error.OperandsMustBeNumbers,
                },
                .PLUS => switch (try self.evaluate(nodes[node + 1].node, env)) {
                    .number => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .number => |rhs| .{ .number = lhs + rhs },
                        else => error.OperandsMustBeNumbers,
                    },
                    .string => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
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
                .BANG => .{ .bool = !(try self.evaluate(nodes[node + 1].node, env)).truthy() },
                .STAR => switch (try self.evaluate(nodes[node + 1].node, env)) {
                    .number => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .number => |rhs| .{ .number = lhs * rhs },
                        else => error.OperandsMustBeNumbers,
                    },
                    else => error.OperandsMustBeNumbers,
                },
                .SLASH => switch (try self.evaluate(nodes[node + 1].node, env)) {
                    .number => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .number => |rhs| .{ .number = lhs / rhs },
                        else => error.OperandsMustBeNumbers,
                    },
                    else => error.OperandsMustBeNumbers,
                },
                .LESS => switch (try self.evaluate(nodes[node + 1].node, env)) {
                    .number => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .number => |rhs| .{ .bool = lhs < rhs },
                        else => error.OperandsMustBeNumbers,
                    },
                    .string => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .string => |rhs| .{ .bool = mem.lessThan(u8, lhs, rhs) },
                        else => error.OperandsMustBeStrings,
                    },
                    else => error.OperandsMustBeNumbers,
                },
                .LESS_EQUAL => switch (try self.evaluate(nodes[node + 1].node, env)) {
                    .number => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .number => |rhs| .{ .bool = lhs <= rhs },
                        else => error.OperandsMustBeNumbers,
                    },
                    .string => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .string => |rhs| .{ .bool = !mem.lessThan(u8, rhs, lhs) },
                        else => error.OperandsMustBeStrings,
                    },
                    else => error.OperandsMustBeNumbers,
                },
                .GREATER => switch (try self.evaluate(nodes[node + 1].node, env)) {
                    .number => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .number => |rhs| .{ .bool = lhs > rhs },
                        else => error.OperandsMustBeNumbers,
                    },
                    else => error.OperandsMustBeNumbers,
                },
                .GREATER_EQUAL => switch (try self.evaluate(nodes[node + 1].node, env)) {
                    .number => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .number => |rhs| .{ .bool = lhs >= rhs },
                        else => error.OperandsMustBeNumbers,
                    },
                    .string => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .string => |rhs| .{ .bool = !mem.lessThan(u8, lhs, rhs) },
                        else => error.OperandsMustBeStrings,
                    },
                    else => error.OperandsMustBeNumbers,
                },
                .EQUAL_EQUAL => .{ .bool = switch (try self.evaluate(nodes[node + 1].node, env)) {
                    .number => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .number => |rhs| lhs == rhs,
                        else => false,
                    },
                    .string => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .string => |rhs| mem.eql(u8, lhs, rhs),
                        else => false,
                    },
                    .bool => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .bool => |rhs| lhs == rhs,
                        else => false,
                    },
                    else => false,
                } },
                .BANG_EQUAL => .{ .bool = switch (try self.evaluate(nodes[node + 1].node, env)) {
                    .number => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .number => |rhs| lhs != rhs,
                        else => true,
                    },
                    .string => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .string => |rhs| !mem.eql(u8, lhs, rhs),
                        else => true,
                    },
                    .bool => |lhs| switch (try self.evaluate(nodes[node + 2].node, env)) {
                        .bool => |rhs| lhs != rhs,
                        else => true,
                    },
                    else => true,
                } },
                .PRINT => .{ .nil = {
                    const value = try self.evaluate(nodes[node + 1].node, env);
                    switch (value) {
                        .function => |func| self.writer.print("<fn {s}>", .{self.tokens[func.nodes[0].head.token].src}) catch {},
                        else => self.writer.print("{any}\n", .{value}) catch {},
                    }
                } },
                .SEMICOLON => .{ .nil = {} },
                .RIGHT_BRACE, .EOF => blk: {
                    const index = node + 1;
                    const count = nodes[node].head.count;
                    var value: Value = undefined;
                    for (nodes[index .. index + count]) |arg|
                        value = try self.evaluate(arg.node, env);
                    break :blk value;
                },
                .VAR => .{ .nil = {
                    try env.first.?.data.put(
                        self.tokens[nodes[node + 1].node].src,
                        switch (nodes[node].head.count) {
                            2 => try self.evaluate(nodes[node + 2].node, env),
                            else => .{ .nil = {} },
                        },
                    );
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
                        .{ slice, token.line(self.source) },
                    );
                    break :blk error.UndefinedVariable;
                },
                .EQUAL => blk: {
                    const lhs = self.tokens[nodes[nodes[node + 1].node].head.token];
                    const key = lhs.src;
                    var it = env.first;
                    while (it) |n| : (it = n.next) {
                        var scope = n.data;
                        if (scope.get(key)) |_| {
                            const val = try self.evaluate(nodes[node + 2].node, env);
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
                .LEFT_BRACE => blk: {
                    const scope = try self.createScope();
                    defer self.destroyScope(scope);
                    env.prepend(scope);
                    defer _ = env.popFirst();
                    if (self.evaluate(nodes[node + 1].node, env)) |res| {
                        break :blk res;
                    } else |err| {
                        switch (err) {
                            error.EarlyExit => try env.first.?.next.?.data.put("", scope.data.get("").?),
                            else => {},
                        }
                        break :blk err;
                    }
                },
                .IF => if ((try self.evaluate(nodes[node + 1].node, env)).truthy())
                    try self.evaluate(nodes[node + 2].node, env)
                else if (nodes[node].head.count > 2)
                    try self.evaluate(nodes[node + 3].node, env)
                else
                    .{ .nil = {} },
                .OR => blk: {
                    const lhs = try self.evaluate(nodes[node + 1].node, env);
                    break :blk if (lhs.truthy()) lhs else try self.evaluate(nodes[node + 2].node, env);
                },
                .AND => blk: {
                    const lhs = try self.evaluate(nodes[node + 1].node, env);
                    break :blk if (!lhs.truthy()) lhs else try self.evaluate(nodes[node + 2].node, env);
                },
                .WHILE => .{ .nil = {
                    while ((try self.evaluate(nodes[node + 1].node, env)).truthy()) {
                        _ = try self.evaluate(nodes[node + 2].node, env);
                    }
                } },
                .FOR => .{ .nil = {
                    _ = try self.evaluate(nodes[node + 1].node, env);
                    while ((try self.evaluate(nodes[node + 2].node, env)).truthy()) {
                        _ = try self.evaluate(nodes[node + 3].node, env);
                        _ = try self.evaluate(nodes[node + 4].node, env);
                    }
                } },
                .RIGHT_PAREN => blk: {
                    const index = node + 1;
                    const count = nodes[node].head.count;
                    const args: []Value = try self.allocator.alloc(Value, count);
                    defer self.allocator.free(args);

                    for (nodes[index .. index + count], args) |arg, *value|
                        value.* = try self.evaluate(arg.node, env);

                    switch (args[0]) {
                        .native => |f| break :blk f(args[1..]),
                        .function => |func| {
                            if (args.len != func.nodes.len - 1) {
                                std.debug.print("Expected {d} arguments but got {d}.\n", .{ func.nodes.len - 2, args.len - 1 });
                                break :blk error.WrongArgCount;
                            }
                            const scope = try self.createScope();
                            defer self.destroyScope(scope);
                            var func_env = ValueMaps{ .first = func.first };
                            func_env.prepend(scope);
                            defer _ = func_env.popFirst();
                            for (func.nodes[1..args.len], args[1..]) |param, arg| {
                                try scope.data.put(self.tokens[param.head.token].src, arg);
                            }
                            if (self.evaluate(func.nodes[args.len].node, &func_env)) |value| {
                                break :blk value;
                            } else |err| break :blk switch (err) {
                                error.EarlyExit => scope.data.get("").?,
                                else => err,
                            };
                        },
                        else => break :blk error.NotAFunction,
                    }
                },
                .FUN => .{ .nil = {
                    const first = env.first.?;
                    const index = node + 1;
                    const count = nodes[node].head.count;
                    try first.data.put(
                        self.tokens[nodes[index].head.token].src,
                        .{ .function = .{ .nodes = nodes[index .. index + count], .first = first } },
                    );
                } },
                .RETURN => blk: {
                    try env.first.?.data.put("", switch (nodes[node].head.count) {
                        0 => .{ .nil = {} },
                        else => try self.evaluate(nodes[node + 1].node, env),
                    });
                    break :blk error.EarlyExit;
                },
                else => unreachable,
            };
        }
    };
}

fn helper(allocator: Allocator, source: []const u8) !Value {
    const tokens = try tokenize.tokens(allocator, source);
    defer allocator.free(tokens);

    var nodes = try std.ArrayListUnmanaged(parse.Node).initCapacity(allocator, 16);
    defer nodes.deinit(allocator);

    const state = try parse.statements(&nodes, allocator, tokens, 0);
    std.debug.assert(state.token == tokens.len - 1);

    const Buffer = std.ArrayListUnmanaged(u8);
    var stdout = try Buffer.initCapacity(allocator, 16);
    defer stdout.deinit(allocator);

    const evaluator = Evaluator(Buffer.Writer).init(
        allocator,
        source,
        tokens,
        nodes.items,
        stdout.writer(allocator),
    );
    var env = try evaluator.createEnv();
    defer evaluator.destroyScope(env.first.?);

    return evaluator.evaluate(state.node.node, &env);
}

test "evaluate" {
    const allocator = std.testing.allocator;
    const cases = [_]struct { source: []const u8, expected: Value }{
        .{ .source = "true", .expected = .{ .bool = true } },
        .{ .source = "!true", .expected = .{ .bool = false } },
        .{ .source = "1 + 1", .expected = .{ .number = 2 } },
        .{ .source = "1 + 2 * 3", .expected = .{ .number = 7 } },
        .{ .source = "2 < 3", .expected = .{ .bool = true } },
        .{ .source = 
        \\ "foo" < "bar"
        , .expected = .{ .bool = false } },
        .{ .source = "{ var a = 1; a; }", .expected = .{ .number = 1 } },
        .{ .source = "var a; a = 1", .expected = .{ .number = 1 } },
        .{ .source = "var a = 0; a = 1", .expected = .{ .number = 1 } },
        .{ .source = 
        \\ {
        \\   fun sumTo(n) {
        \\     var sum = 0;
        \\     for (var i = 0; i < n; i = i + 1) {
        \\       sum = sum + i;
        \\     }
        \\     return sum;
        \\   }
        \\   sumTo(6);
        \\ }
        , .expected = .{ .number = 15 } },
    };
    for (cases) |case| {
        const actual = try helper(allocator, case.source);
        try testing.expect(std.meta.eql(case.expected, actual));
    }
}
