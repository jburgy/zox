const std = @import("std");
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Parser = @import("parser.zig").Parser;

const stdout = std.io.getStdOut().writer();

pub fn main() !void {
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

    if (!tokenize and !parse) {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(1);
    }

    const allocator = std.heap.page_allocator;
    const file_contents = try std.fs.cwd().readFileAlloc(allocator, filename, std.math.maxInt(usize));
    defer allocator.free(file_contents);

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
            allocator.free(file_contents);
            std.process.exit(65);
        }
    } else if (parse and file_contents.len > 0) {
        var tokens = Tokenizer.init(file_contents);
        var parser = Parser.init(allocator, &tokens);
        if (parser.expression()) |expr| {
            try expr.emit(file_contents, stdout);
        } else |err| switch (err) {
            error.UnexpectedToken => {
                const token = parser.peek();
                const line = std.mem.count(u8, file_contents[0..token.loc.end], "\n");
                std.debug.print(
                    "[Line {d}] Error at '{s}': Expect expression.",
                    .{ line + 1, file_contents[token.loc.start..token.loc.end] },
                );
                allocator.free(file_contents);
                std.process.exit(65);
            },
            else => unreachable,
        }
    } else {
        try stdout.print("EOF  null\n", .{}); // Placeholder, remove this line when implementing the scanner
    }
}
