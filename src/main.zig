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
    var arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer arena.deinit();
    const gpa = arena.allocator();

    const source =
        \\"b" in "abc";
        // \\[1, 2, 3];
        // \\{"one": 1, "two": 2};
        //
        // \\an_int = 4 / 2;
        // \\the_int = 2^3;
        // \\
        // \\
        // \\def add(a, b) {
        // \\    sum = a + b;
        // \\    return sum;
        // \\}
        // \\
        // \\int_sum = add(a_int, the_int);
        // \\print("Success") if c > 0 else print(0);
        // \\
        // \\0 if true and the_int - an_int else int_sum or "Huh?";
        // \\
        // \\a_list = [1, 2, 3];
        // \\a_dict = {"integer": 1, "list": [2, 3]};
        // \\the_list = [0, {"one": 1}, 2 + 3];
        // \\
        // \\zero = the_list[a_list[0]];
        // \\
        // \\for n in a_list {
        // \\    print(n + 1);
        // \\}
        // \\
        // \\zero_in_the_list = 0 in the_list;
        // \\
        // \\selector = Select([1, 2, 3], [3, 2, 1], !=);
        // \\
        // \\list_comp = [i + 1 if i > 0 else i for i in a_list];
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

    var interpreter: Interpreter = .init(tree, gpa);
    const ivalue = try interpreter.visitNode(2);
    std.debug.print("{any}\n", .{ivalue});
    // for (0..ivalue.list.elems.len) |i|
    //     std.debug.print("{any}\n", .{ivalue.list.elems[i].*});
    // const query = &IValue{ .string = "two" };
    // std.debug.print("{any}\n", .{ivalue.map.get(@constCast(query))});
}
