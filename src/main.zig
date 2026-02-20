const std = @import("std");
const heap = std.heap;

const Tokenizer = @import("Tokenizer.zig");
const ast = @import("ast.zig");
const Parser = ast.Parser;
const Node = Parser.Node;
const NodeIndex = Parser.NodeIndex;
const Renderer = @import("Renderer.zig");
const Interpreter = @import("Interpreter.zig");
const IValue = Interpreter.IValue;

pub fn main() !void {
    var arena: heap.ArenaAllocator = .init(heap.page_allocator);
    defer arena.deinit();
    const gpa = arena.allocator();

    const source =
        // \\ (Indices * 3)("Hello!");
        // \\selector = Select([1, 2, 2], [0, 1, 2], ==);
        // \\Aggregate(selector, [4, 6, 8]);
        \\indices = Indices("Hello!");
        \\Print(indices);
    ;

    var tokenizer: Tokenizer = .init(source);
    var parser: Parser = try .init(&tokenizer, gpa);
    const tree = parser.buildTree() catch |err| {
        const diagnostic = parser.diagnostic.?;
        std.debug.print(
            "Error at line {d}, column {d}: {any}\n",
            .{ diagnostic.at.line, diagnostic.at.column, diagnostic.expected },
        );
        return err;
    };

    var buffer: [1024]u8 = undefined;
    const writer = std.Progress.lockStderrWriter(&buffer);
    defer std.Progress.unlockStderrWriter();

    var renderer: Renderer = .init(writer, tree.nodes, tree.adpb);
    try writer.print("Parsed AST (index-backed):\n", .{});
    for (tree.indices) |node| try renderer.render(node);

    var interpreter: Interpreter = try .init(gpa, writer, tree);
    defer interpreter.deinit();
    const ivalue = interpreter.walkTree() catch |err| {
        const diagnostic = interpreter.diagnostic.?;
        std.debug.print(
            "Error at line {d}: {s}\n",
            .{ diagnostic.at, diagnostic.description },
        );
        return err;
    };
    try writer.print("{any}\n", .{ivalue});

    // for (0..ivalue.imatrix.rows) |row| {
    //     for (0..ivalue.imatrix.columns) |column| {
    //         const elem = ivalue.imatrix.get(@intCast(row), @intCast(column));
    //         std.debug.print(
    //             "({d}, {d}): {}\n",
    //             .{ row, column, elem.boolean },
    //         );
    //     }
    // }
}
