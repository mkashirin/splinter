const std = @import("std");

const Tokenizer = @import("Tokenizer.zig");
const ast = @import("ast.zig");
const Parser = ast.Parser;
const Node = Parser.Node;
const NodeIndex = Parser.NodeIndex;
const Renderer = @import("Renderer.zig");
const Interpreter = @import("Interpreter.zig");
const IValue = Interpreter.IValue;

pub fn main() !void {
    // var da: std.heap.DebugAllocator(.{}) = .init;
    // defer _ = da.deinit();
    // const gpa = da.allocator();
    var arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer arena.deinit();
    const gpa = arena.allocator();

    const source =
        \\a_list = [1, 2, 3];
        \\for n in a_list {
        \\    print(n + 1);
        \\}
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

    var buffer: [1024]u8 = undefined;
    const writer = std.Progress.lockStderrWriter(&buffer);
    defer std.Progress.unlockStderrWriter();

    var renderer: Renderer = .init(writer, tree.nodes, tree.adpb);
    std.debug.print("Parsed AST (index-backed):\n", .{});
    for (tree.indices) |node| try renderer.render(node);

    var interpreter: Interpreter = try .init(tree, gpa);
    defer interpreter.deinit();
    const ivalue = try interpreter.walkTree();
    _ = ivalue;
    // std.debug.print("{any}\n", .{ivalue.list.elems[0]});
}
