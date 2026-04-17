// TODO: Implement floating-point numbers fast!

const std = @import("std");
const heap = std.heap;

const Tokenizer = @import("Tokenizer.zig");
const ast = @import("ast.zig");
const Renderer = @import("Renderer.zig");
const Interpreter = @import("Interpreter.zig");

pub fn main(init: std.process.Init) !void {
    var arena: heap.ArenaAllocator = .init(heap.page_allocator);
    defer arena.deinit();
    const gpa = arena.allocator();

    var writer = std.Io.File.stderr().writer(init.io, &.{});
    const stderr = &writer.interface;

    var file = try std.Io.Dir.openFile(.cwd(), init.io, "main.l", .{});
    defer file.close(init.io);
    var source: [1024]u8 = undefined;
    const read = try file.readPositionalAll(init.io, &source, 0);

    var tokenizer: Tokenizer = .init(source[0..read]);
    var parser: ast.Parser = try .init(&tokenizer, gpa);
    const tree = parser.buildTree() catch |err| {
        const diagnostic = parser.diagnostic.?;
        try stderr.print(
            "Error at line {d}, column {d}: {any}\n",
            .{ diagnostic.at.line, diagnostic.at.column, diagnostic.expected },
        );
        return err;
    };

    var renderer: Renderer = .init(stderr, tree.nodes, tree.adpb);
    try stderr.print("Parsed AST (index-backed):\n", .{});
    for (tree.indices) |node| try renderer.render(node);

    var interpreter: Interpreter = try .init(gpa, stderr, tree);
    defer interpreter.deinit();
    const ivalue = interpreter.walkTree() catch |err| {
        const diagnostic = interpreter.diagnostic.?;
        try stderr.print("Error at line {d}: {s}\n", .{ diagnostic.at, diagnostic.description });
        return err;
    };
    try stderr.print("{any}\n", .{ivalue});

    // for (0..ivalue.imatrix.rows) |row| {
    //     for (0..ivalue.imatrix.columns) |column| {
    //         const elem = ivalue.imatrix.get(@intCast(row), @intCast(column));
    //         std.debug.print("({d}, {d}): {}\n", .{ row, column, elem.boolean });
    //     }
    // }
}
