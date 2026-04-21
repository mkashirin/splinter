writer: *std.Io.Writer,
nodes: []const Node,
indent_level: u8,
adpb: []const Index,
/// Renderer is a struct that pretty-prints an AST to a generic writer. It
/// separates the printing logic from the AST data structures.
const Renderer = @This();

const INDENT_SIZE = 4;

pub fn init(writer: *std.Io.Writer, nodes: []const Node, adpb: []const Index) Renderer {
    return .{ .writer = writer, .nodes = nodes, .indent_level = 0, .adpb = adpb };
}

pub fn render(r: *Renderer, root_index: Index) void {
    r.rnode(root_index);
}

fn indent(r: *Renderer) void {
    r.indent_level += INDENT_SIZE;
}

fn unindent(r: *Renderer) void {
    r.indent_level -= INDENT_SIZE;
}

fn print(r: *Renderer, comptime format: []const u8, args: anytype) void {
    for (0..r.indent_level) |_| r.writer.print(" ", .{}) catch unreachable;
    r.writer.print(format ++ "\n", args) catch unreachable;
}

fn rnode(r: *Renderer, index: Index) void {
    r.print("(#{d})", .{index});
    switch (r.nodes[@intCast(index)]) {
        .boolean => |boolean_| r.boolean(boolean_),
        .float => |float_| r.float(float_),
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
        .call => |call_| r.call(call_),
        .for_stmt => |for_stmt| r.forStmt(for_stmt),
        .op_arg => |op_arg| r.opArg(op_arg),
    }
}

fn boolean(r: *Renderer, boolean_: bool) void {
    r.print("Bool({any})", .{boolean_});
}

fn float(r: *Renderer, float_: f64) void {
    r.print("Float({d})", .{float_});
}

fn int(r: *Renderer, int_: i32) void {
    r.print("Int({d})", .{int_});
}

fn string(r: *Renderer, string_: []const u8) void {
    r.print("String(\"{s}\")", .{string_});
}

fn ident(r: *Renderer, ident_: []const u8) void {
    r.print("Ident({s})", .{ident_});
}

fn list(r: *Renderer, list_: ast.List) void {
    r.print("List:", .{});
    r.indent();
    defer r.unindent();

    const end = list_.elems.len;
    for (0..end) |i| r.rnode(list_.elems[i]);
}

fn listComp(r: *Renderer, list_comp: ast.ListComp) void {
    r.print("ListComp:", .{});
    r.indent();
    defer r.unindent();

    r.print("Expr:", .{});
    {
        r.indent();
        defer r.unindent();
        r.rnode(list_comp.expr);
    }

    r.print("Variable: {s}", .{list_comp.variable});

    r.print("Iter:", .{});
    {
        r.indent();
        defer r.unindent();
        r.rnode(list_comp.iter);
    }
}

fn hashMap(r: *Renderer, hash_map: ast.HashMap) void {
    r.print("HashMap:", .{});
    r.indent();
    defer r.unindent();

    const keys_end = hash_map.keys.len;
    const values_end = hash_map.values.len;
    for (0..keys_end, 0..values_end) |i, j| {
        r.print("Pair:", .{});
        r.indent();
        defer r.unindent();
        r.rnode(hash_map.keys[i]);
        r.rnode(hash_map.values[j]);
    }
}

fn binExpr(r: *Renderer, bin_expr: ast.BinExpr) void {
    r.print("BinExpr({s}):", .{bols.get(bin_expr.op)});
    r.indent();
    defer r.unindent();
    r.rnode(bin_expr.lhs);
    r.rnode(bin_expr.rhs);
}

fn condExpr(r: *Renderer, cond_expr: ast.CondExpr) void {
    r.print("CondExpr:", .{});
    r.indent();
    defer r.unindent();

    r.print("Then:", .{});
    {
        r.indent();
        defer r.unindent();
        r.rnode(cond_expr.then);
    }

    r.print("If:", .{});
    {
        r.indent();
        defer r.unindent();
        r.rnode(cond_expr.if_cond);
    }

    r.print("Else:", .{});
    {
        r.indent();
        defer r.unindent();
        r.rnode(cond_expr.else_expr);
    }
}

fn indexExpr(r: *Renderer, index_expr: ast.IndexExpr) void {
    r.print("IndexExpr:", .{});
    r.indent();
    defer r.unindent();

    r.print("Target:", .{});
    {
        r.indent();
        defer r.unindent();
        r.rnode(index_expr.target);
    }

    r.print("Index:", .{});
    {
        r.indent();
        defer r.unindent();
        r.rnode(index_expr.index);
    }
}

fn assignStmt(r: *Renderer, assign_stmt: ast.AssignStmt) void {
    r.print("AssignStmt(name: {s}):", .{assign_stmt.name});
    r.indent();
    defer r.unindent();
    r.rnode(assign_stmt.value);
}

fn fnDef(r: *Renderer, fn_def: ast.FnDef) void {
    r.print("FnDef(name: {s})", .{fn_def.name});
    r.indent();
    defer r.unindent();

    r.print("Args:", .{});
    {
        r.indent();
        defer r.unindent();
        const args_start: usize = @intCast(fn_def.args_start);
        const args_end = args_start + @as(usize, fn_def.args_len);
        var i: usize = args_start;
        while (i < args_end) : (i += 1) {
            const arg_node_index = r.adpb[i];
            const arg_node = r.nodes[@intCast(arg_node_index)];
            // r.rnode(@intCast(arg_node_index));
            r.print("{s}", .{arg_node.ident});
        }
    }

    r.print("Body:", .{});
    {
        r.indent();
        defer r.unindent();
        const body_start: usize = @intCast(fn_def.body_start);
        const body_end = body_start + @as(usize, fn_def.body_len);
        var i: usize = body_start;
        while (i < body_end) : (i += 1) r.rnode(r.adpb[i]);
    }
}

fn returnStmt(r: *Renderer, return_stmt: ast.ReturnStmt) void {
    r.print("ReturnStmt:", .{});
    r.indent();
    defer r.unindent();
    r.rnode(return_stmt.value);
}

fn call(r: *Renderer, call_: ast.Call) void {
    switch (call_.callable) {
        .ident => |ident_| r.print("Call(name: {s}):", .{ident_}),
        .expr => |expr| {
            r.print("Call(expr:", .{});
            r.indent();
            r.rnode(expr);
            r.unindent();
            r.print(")", .{});
        },
    }
    r.indent();
    defer r.unindent();

    r.print("Args:", .{});
    {
        r.indent();
        defer r.unindent();
        if (call_.args_len < 1) r.print("None", .{});
        const args_start: usize = @intCast(call_.args_start);
        const args_end = args_start + @as(usize, call_.args_len);
        var i: usize = args_start;
        while (i < args_end) : (i += 1) r.rnode(r.adpb[i]);
    }
}

fn opArg(r: *Renderer, op_arg: ast.BinOp) void {
    r.print("BinOp({s})", .{bols.get(op_arg)});
    r.indent();
    defer r.unindent();
}

fn forStmt(r: *Renderer, for_stmt: ast.ForStmt) void {
    r.print("ForStmt(var: {s}):", .{for_stmt.variable});
    r.indent();
    defer r.unindent();

    r.print("Iter:", .{});
    {
        r.indent();
        defer r.unindent();
        r.rnode(for_stmt.iter);
    }

    r.print("Body:", .{});
    {
        r.indent();
        defer r.unindent();
        const body_start: usize = @intCast(for_stmt.body_start);
        const body_end = body_start + @as(usize, for_stmt.body_len);
        var i: usize = body_start;
        while (i < body_end) : (i += 1) r.rnode(r.adpb[i]);
    }
}

const bols: std.enums.EnumArray(ast.BinOp, []const u8) = .init(.{
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
