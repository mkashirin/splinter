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

    var buffer: [1024]u8 = undefined;
    const writer = std.Progress.lockStderrWriter(&buffer);
    defer std.Progress.unlockStderrWriter();

    const source =
        // \\def add(a, b) {
        // \\    return a + b;
        // \\}
        // \\a_list = [1, 2, 3];
        // \\lazy = (2 * add if a_list[1] else 0 * add);
        // \\evaluated = lazy(2, 2);
        // \\Print(evaluated);
        // \\comprehension = [i + "d" for i in {"a": 0, "b": 1}];
        // \\Print(comprehension[1]);
        // TODO: For now, it returns an array of booleans (result of `Tokens == "e"`), when it
        // really should be returning `Indicator(...)`.
        \\matches = (Indicator(Tokens == "e"))("Hey");
        \\indicated = matches[2];
        \\Print(indicated);
    ;

    var tokenizer: Tokenizer = .init(source);
    var parser: Parser = try .init(&tokenizer, gpa);
    const tree = parser.buildTree() catch |err| {
        const diagnostic = parser.diagnostic.?;
        try writer.print(
            "Error at line {d}, column {d}: {any}\n",
            .{ diagnostic.at.line, diagnostic.at.column, diagnostic.expected },
        );
        return err;
    };

    var renderer: Renderer = .init(writer, tree.nodes, tree.adpb);
    try writer.print("Parsed AST (index-backed):\n", .{});
    for (tree.indices) |node| try renderer.render(node);

    var interpreter: Interpreter = try .init(gpa, writer, tree);
    defer interpreter.deinit();
    const ivalue = interpreter.walkTree() catch |err| {
        const diagnostic = interpreter.diagnostic.?;
        try writer.print("Error at line {d}: {s}\n", .{ diagnostic.at, diagnostic.description });
        return err;
    };
    try writer.print("{any}\n", .{ivalue});

    // for (0..ivalue.imatrix.rows) |row| {
    //     for (0..ivalue.imatrix.columns) |column| {
    //         const elem = ivalue.imatrix.get(@intCast(row), @intCast(column));
    //         std.debug.print("({d}, {d}): {}\n", .{ row, column, elem.boolean });
    //     }
    // }
}
