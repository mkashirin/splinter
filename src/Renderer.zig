/// Renderer is a stateful struct that pretty-prints an AST to a generic
/// writer. It separates the printing logic from the AST data structures.
writer: *std.Io.Writer,
nodes: []const Node,
indent_level: u8,
adpb: []const Index,
const Renderer = @This();

const INDENT_SIZE = 4;

pub fn init(
    writer: *std.Io.Writer,
    nodes: []const Node,
    adp: []const Index,
) Renderer {
    return .{
        .writer = writer,
        .nodes = nodes,
        .indent_level = 0,
        .adpb = adp,
    };
}

pub fn render(r: *Renderer, root_index: Index) !void {
    try r.renderNode(root_index);
}

fn indent(r: *Renderer) void {
    r.indent_level += INDENT_SIZE;
}

fn unindent(r: *Renderer) void {
    r.indent_level -= INDENT_SIZE;
}

fn print(
    r: *Renderer,
    comptime format: []const u8,
    args: anytype,
) !void {
    for (0..r.indent_level) |_| try r.writer.print(" ", .{});
    try r.writer.print(format ++ "\n", args);
}

fn renderNode(r: *Renderer, index: Index) std.Io.Writer.Error!void {
    try r.print("(#{d})", .{index});
    try switch (r.nodes[@intCast(index)]) {
        .boolean => |boolean_| r.boolean(boolean_),
        .int => |int_| r.int(int_),
        .string => |string_| r.string(string_),
        .ident => |ident_| r.ident(ident_),
        .list => |list_| r.list(list_),
        .list_comp => |list_comp| r.listComp(list_comp),
        .hash_map => |hash_map| r.hashMap(hash_map),

        .bin_expr => |bin_expr| r.binExpr(bin_expr),
        .cond_expr => |cond_expr| r.condExpr(cond_expr),
        .index_expr => |index_expr| r.indexExpr(index_expr),

        .assign_stmt => |assign_stmt| r.assignStmt(assign_stmt),
        .fn_def => |fn_def| r.fnDef(fn_def),
        .return_stmt => |return_stmt| r.returnStmt(return_stmt),
        .fn_call => |fn_call| r.fnCall(fn_call),
        .for_stmt => |for_stmt| r.forStmt(for_stmt),
        .op_arg => |op_arg| r.opArg(op_arg),
    };
}

fn boolean(r: *Renderer, boolean_: bool) !void {
    try r.print("Bool({any})", .{boolean_});
}

fn int(r: *Renderer, int_: i64) !void {
    try r.print("Int({d})", .{int_});
}

fn string(r: *Renderer, string_: []const u8) !void {
    try r.print("String(\"{s}\")", .{string_});
}

fn ident(r: *Renderer, ident_: []const u8) !void {
    try r.print("Ident({s})", .{ident_});
}

fn list(r: *Renderer, list_: ast.List) !void {
    try r.print("List:", .{});
    r.indent();
    defer r.unindent();

    const end = list_.elems.len;
    for (0..end) |i| try r.renderNode(list_.elems[i]);
}

fn listComp(r: *Renderer, list_comp: ast.ListComp) !void {
    try r.print("ListComp:", .{});
    r.indent();
    defer r.unindent();

    try r.print("Expr:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(list_comp.expr);
    }

    try r.print("Variable: {s}", .{list_comp.variable});

    try r.print("Iterable:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(list_comp.iterable);
    }
}

fn hashMap(r: *Renderer, hash_map: ast.HashMap) !void {
    try r.print("HashMap:", .{});
    r.indent();
    defer r.unindent();

    const keys_end = hash_map.keys.len;
    const values_end = hash_map.values.len;
    for (0..keys_end, 0..values_end) |i, j| {
        try r.print("Pair:", .{});
        r.indent();
        defer r.unindent();
        try r.renderNode(hash_map.keys[i]);
        try r.renderNode(hash_map.values[j]);
    }
}

fn binExpr(r: *Renderer, bin_expr: ast.BinExpr) !void {
    try r.print("BinExpr({s}):", .{bol.get(bin_expr.op)});
    r.indent();
    defer r.unindent();
    try r.renderNode(bin_expr.lhs);
    try r.renderNode(bin_expr.rhs);
}

fn condExpr(r: *Renderer, cond_expr: ast.CondExpr) !void {
    try r.print("CondExpr:", .{});
    r.indent();
    defer r.unindent();

    try r.print("Then:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(cond_expr.then);
    }

    try r.print("If:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(cond_expr.if_cond);
    }

    try r.print("Else:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(cond_expr.else_expr);
    }
}

fn indexExpr(r: *Renderer, index_expr: ast.IndexExpr) !void {
    try r.print("IndexExpr:", .{});
    r.indent();
    defer r.unindent();

    try r.print("Target:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(index_expr.target);
    }

    try r.print("Index:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(index_expr.index);
    }
}

fn assignStmt(r: *Renderer, assign_stmt: ast.AssignStmt) !void {
    try r.print("AssignStmt(name: {s}):", .{assign_stmt.name});
    r.indent();
    defer r.unindent();
    try r.renderNode(assign_stmt.value);
}

fn fnDef(r: *Renderer, fn_def: ast.FnDef) !void {
    try r.print("FnDef(name: {s})", .{fn_def.name});
    r.indent();
    defer r.unindent();

    try r.print("Args:", .{});
    {
        r.indent();
        defer r.unindent();
        const args_start: usize = @intCast(fn_def.args_start);
        const args_end = args_start + @as(usize, fn_def.args_len);
        var i: usize = args_start;
        while (i < args_end) : (i += 1) {
            const arg_node_index = r.adpb[i];
            const arg_node = r.nodes[@intCast(arg_node_index)];
            try r.print("{s}", .{arg_node.ident});
        }
    }

    try r.print("Body:", .{});
    {
        r.indent();
        defer r.unindent();
        const body_start: usize = @intCast(fn_def.body_start);
        const body_end = body_start + @as(usize, fn_def.body_len);
        var i: usize = body_start;
        while (i < body_end) : (i += 1) try r.renderNode(r.adpb[i]);
    }
}

fn returnStmt(r: *Renderer, return_stmt: ast.ReturnStmt) !void {
    try r.print("ReturnStmt:", .{});
    r.indent();
    defer r.unindent();
    try r.renderNode(return_stmt.value);
}

fn fnCall(r: *Renderer, fn_call: ast.FnCall) !void {
    try r.print("FnCall(name: {s}):", .{fn_call.name});
    r.indent();
    defer r.unindent();

    try r.print("Args:", .{});
    {
        r.indent();
        defer r.unindent();
        const args_start: usize = @intCast(fn_call.args_start);
        const args_end = args_start + @as(usize, fn_call.args_len);
        var i: usize = args_start;
        while (i < args_end) : (i += 1) try r.renderNode(r.adpb[i]);
    }
}

fn opArg(r: *Renderer, op_arg: ast.BinOp) !void {
    try r.print("BinOp({s})", .{bol.get(op_arg)});
    r.indent();
    defer r.unindent();
}

fn forStmt(r: *Renderer, for_stmt: ast.ForStmt) !void {
    try r.print("ForStmt(var: {s}):", .{for_stmt.variable});
    r.indent();
    defer r.unindent();

    try r.print("Iterable:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(for_stmt.iterable);
    }

    try r.print("Body:", .{});
    {
        r.indent();
        defer r.unindent();
        const body_start: usize = @intCast(for_stmt.body_start);
        const body_end = body_start + @as(usize, for_stmt.body_len);
        var i: usize = body_start;
        while (i < body_end) : (i += 1) try r.renderNode(r.adpb[i]);
    }
}

const bol: std.enums.EnumArray(ast.BinOp, []const u8) = .init(.{
    // BOL — Binary Operations' Lexemes.
    // zig fmt: off
    .add    = "+",
    .subtr  = "-",
    .mult   = "*",
    .power  = "^",
    .div    = "/",

    .equal                  = "==",
    .not_equal              = "!=",
    .greater_than           = ">",
    .greater_or_equal_than  = ">=",
    .less_than              = "<",
    .less_or_equal_than     = "<=",

    .logic_and  = "and",
    .logic_or   = "or",
    .is_in      = "in",
    // zig fmt: on
});

test {
    _ = @import("Renderer.zig");
}

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Node = ast.Node;
const Index = ast.Index;
