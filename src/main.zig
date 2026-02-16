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
        // \\selector = Select([1, 2, 2], [0, 1, 2], ==);
        // \\Aggregate(selector, [4, 6, 8]);
        \\Indices("Hello!");
    ;

    var tokenizer: Tokenizer = .init(source);
    var parser: Parser = try .init(&tokenizer, gpa);
    var tree = parser.buildTree() catch |err| {
        const err_location = parser.current.location;
        const diagnostic = parser.diagnostic.?;
        std.debug.print(
            "Error at line {d}, column {d}: {any}\n",
            .{ err_location.line, err_location.column, diagnostic.expected },
        );
        parser.deinit();
        return err;
    };
    defer tree.deinit(gpa);

    // var buffer: [1024]u8 = undefined;
    // const writer = std.Progress.lockStderrWriter(&buffer);
    // defer std.Progress.unlockStderrWriter();

    // var renderer: Renderer = .init(writer, tree.nodes, tree.adpb);
    // std.debug.print("Parsed AST (index-backed):\n", .{});
    // for (tree.indices) |node| try renderer.render(node);

    var interpreter: Interpreter = try .init(tree, gpa);
    defer interpreter.deinit();
    const ivalue = try interpreter.walkTree();
    std.debug.print("{any}\n", .{ivalue.list.elems});

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
