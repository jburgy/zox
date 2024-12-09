const std = @import("std");
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Parser = @import("parser.zig").Parser;
const ValueMap = @import("parser.zig").ValueMap;

const stdout = std.io.getStdOut().writer();

pub fn main() !u8 {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    std.debug.print("Logs from your program will appear here!\n", .{});

    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh tokenize <filename>\n", .{});
        std.process.exit(1);
    }

    const command = args[1];
    const filename = args[2];
    const tokenize = std.mem.eql(u8, command, "tokenize");
    const parse = std.mem.eql(u8, command, "parse");
    const evaluate = std.mem.eql(u8, command, "evaluate");
    const run = std.mem.eql(u8, command, "run");

    if (!(tokenize or parse or evaluate or run)) {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(1);
    }

    const allocator = std.heap.page_allocator;
    const file_contents = try std.fs.cwd().readFileAlloc(allocator, filename, std.math.maxInt(usize));
    defer allocator.free(file_contents);

    var status: u8 = 0;

    // Uncomment this block to pass the first stage
    if (tokenize and file_contents.len > 0) {
        var tokenizer = Tokenizer.init(file_contents);
        var lexical_error = false;
        scan: while (true) {
            const token = tokenizer.next();
            if (try tokenizer.dump(token))
                lexical_error = true;
            switch (token.tag) {
                .EOF => break :scan,
                else => {},
            }
        }
        if (lexical_error) {
            status = 65;
        }
    } else if ((parse or evaluate or run) and file_contents.len > 0) {
        var tokens = Tokenizer.init(file_contents);
        var parser = Parser.init(allocator, &tokens);
        var env = ValueMap.init(allocator);
        if (if (evaluate) parser.expression() else parser.statements()) |expr| {
            if (parse) {
                try expr.emit(file_contents, stdout);
            } else if (expr.evaluate(file_contents, allocator, &env)) |value| {
                if (evaluate)
                    try stdout.print("{any}", .{value});
            } else |err| {
                switch (err) {
                    error.OperandMustBeANumber => std.debug.print(
                        "Operand must be a number.\n[Line {d}]",
                        .{parser.peek().line(file_contents)},
                    ),
                    error.OperandsMustBeNumbers => std.debug.print(
                        "Operands must be numbers.\n[Line {d}]",
                        .{parser.peek().line(file_contents)},
                    ),
                    else => unreachable,
                }
                status = 70;
            }
        } else |err| {
            switch (err) {
                error.UnexpectedToken => {
                    const token = parser.peek();
                    std.debug.print(
                        "[Line {d}] Error at '{s}': Expect expression.",
                        .{ token.line(file_contents), token.source(file_contents) },
                    );
                    status = 65;
                },
                else => unreachable,
            }
        }
    } else {
        try stdout.print("EOF  null\n", .{}); // Placeholder, remove this line when implementing the scanner
    }
    return status;
}
