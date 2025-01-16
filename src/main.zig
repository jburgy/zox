const std = @import("std");
const testing = std.testing;
const evaluate = @import("evaluate.zig").evaluate;

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

pub fn main() !u8 {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    std.debug.print("Logs from your program will appear here!\n", .{});

    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: ./your_program.sh <filename>\n", .{});
        std.process.exit(1);
    }

    const filename = args[1];

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();
    const file_contents = try std.fs.cwd().readFileAlloc(
        allocator,
        filename,
        std.math.maxInt(usize),
    );
    defer allocator.free(file_contents);

    var status: u8 = 0;
    if (file_contents.len == 0) return status;

    if (evaluate.evaluate(allocator, file_contents, stdout)) |value| {
        try stdout.print("{any}", .{value});
    } else |err| {
        status = 70;
        switch (err) {
            error.UnexpectedToken => {
                status = 65;
            },
            error.OperandMustBeANumber => std.debug.print("Operand must be a number.", .{}),
            error.OperandsMustBeNumbers => std.debug.print("Operands must be numbers.", .{}),
            error.OperandsMustBeStrings => std.debug.print("Operands must be strings.", .{}),
            else => {},
        }
    }
    return status;
}

test "recursion" {
    const source =
        \\fun fib(n) {
        \\  if (n < 2) return n;
        \\  return fib(n - 2) + fib(n - 1);
        \\}
        \\fib(19);
    ;

    try testing.expect(switch (try evaluate(testing.allocator, source, stdout)) {
        .number => |num| num == 4181,
        else => false,
    });
}
