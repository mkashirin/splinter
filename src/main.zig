// TODO: Implement floating-point numbers fast!

const std = @import("std");
const heap = std.heap;
const Allocator = std.mem.Allocator;
const Io = std.Io;

const Tokenizer = @import("Tokenizer.zig");
const ast = @import("ast.zig");
const Renderer = @import("Renderer.zig");
const Interpreter = @import("Interpreter.zig");

pub fn main(init: std.process.Init) !void {
    var arena: heap.ArenaAllocator = .init(heap.page_allocator);
    defer arena.deinit();
    const arena_ = arena.allocator();

    var writer = Io.File.stderr().writer(init.io, &.{});
    const stderr = &writer.interface;
    var args = try init.minimal.args.iterateAllocator(arena_);
    _ = args.next();

    var file = try Io.Dir.openFile(.cwd(), init.io, args.next().?, .{});
    defer file.close(init.io);
    var source: [1024]u8 = undefined;
    const read = try file.readPositionalAll(init.io, &source, 0);

    const render_ast = args.next();
    const render_ast_ = if (render_ast != null and
        std.mem.eql(u8, render_ast.?, "--render-ast")) true else false;
    return run(arena_, source[0..read], stderr, render_ast_);
}

fn run(arena: std.mem.Allocator, source: []const u8, stderr: *Io.Writer, render_ast: bool) !void {
    var tokenizer: Tokenizer = .init(source);
    var parser: ast.Parser = try .init(&tokenizer, arena);
    const tree = parser.buildTree() catch |err| {
        const diagnostic = parser.diagnostic.?;
        try stderr.print(
            "Error at line {d}, column {d}: {any}\n",
            .{ diagnostic.at.line, diagnostic.at.column, diagnostic.expected },
        );
        return err;
    };

    if (render_ast) {
        var renderer: Renderer = .init(stderr, tree.nodes, tree.adpb);
        try stderr.print("Parsed AST (index-backed):\n", .{});
        for (tree.indices) |node| try renderer.render(node);
        try stderr.print("\n", .{});
    }

    var interpreter: Interpreter = try .init(arena, stderr, tree);
    defer interpreter.deinit();
    const ivalue = interpreter.walkTree() catch |err| {
        const diagnostic = interpreter.diagnostic.?;
        try stderr.print("Error at line {d}: {s}\n", .{ diagnostic.at, diagnostic.description });
        return err;
    };
    try stderr.print("Last IValue returned: {any}\n", .{ivalue});
}
