// FIXME: Fix the segmentation fault drama.

tree: Tree,
gpa: Allocator,
global: Table,
local: Table,
return_value: IValue = .none,
last_value: IValue = .none,
const Interpreter = @This();

pub fn walkTree(i: *Interpreter) !IValue {
    for (i.tree.indices) |stmt_index| {
        i.last_value.clear(i.gpa);

        i.last_value = i.visitNode(stmt_index) catch |err| switch (err) {
            error.ReturnTrigger => {
                const ivalue = i.return_value;
                i.return_value = .none;
                return ivalue;
            },
            else => return err,
        };
    }

    return i.last_value;
}

pub fn init(tree: Tree, gpa: Allocator) !Interpreter {
    var i: Interpreter = .{
        .tree = tree,
        .gpa = gpa,
        .global = .init(gpa),
        .local = .init(gpa),
    };
    const print_builtin: *IValue = try .init(i.gpa);
    print_builtin.* = .{ .builtin = builtinPrint };
    try i.global.put("print", print_builtin);
    return i;
}

pub fn deinit(i: *Interpreter) void {
    if (i.global.count() > 0) {
        var global_it = i.global.valueIterator();
        while (global_it.next()) |value_ptr| {
            const ivalue_ptr = value_ptr.*;
            std.debug.print("Deiniting from globals: {any}\n", .{ivalue_ptr.*});
            ivalue_ptr.deinit(i.gpa);
        }
    }
    i.global.deinit();

    if (i.local.count() > 0) {
        var local_it = i.local.valueIterator();
        while (local_it.next()) |value_ptr| {
            const ivalue_ptr = value_ptr.*;
            std.debug.print("Deiniting from locals: {any}\n", .{ivalue_ptr.*});
            ivalue_ptr.deinit(i.gpa);
        }
    }
    i.local.deinit();

    // var last_value = i.last_value.?;
    // std.debug.print("Last value: {any}\n", .{i.last_value});
    // i.last_value.clear(i.gpa);
}

const IValuesList = std.ArrayList(*IValue);

pub const IValue = union(enum) {
    int: i64,
    string: []const u8,
    boolean: bool,
    list: List,
    hash_map: HashMap,
    none: void,
    builtin: *const fn (*Interpreter, []*IValue) anyerror!IValue,
    function: u32,
    const Self = @This();

    pub fn init(gpa: Allocator) !*Self {
        return gpa.create(Self);
    }

    pub fn clear(self: *Self, gpa: Allocator) void {
        switch (self.*) {
            .list => |*list| list.deinit(gpa),
            .hash_map => |*hash_map| hash_map.deinit(gpa),
            else => self.* = .none,
        }
    }

    pub fn clone(self: *Self, gpa: Allocator) !Self {
        return switch (self) {
            .list => |list| .{ .list = try list.clone(gpa) },
            .hash_map => |hash_map| .{ .hash_map = try hash_map.clone(gpa) },
            else => self,
        };
    }

    pub fn deinit(self: *Self, gpa: Allocator) void {
        std.debug.print("Called deinit on value: {any}\n", .{self});
        self.clear(gpa);
        gpa.destroy(self);
    }
};

const List = struct {
    elems: []const *IValue,
    const Self = @This();

    pub fn init(gpa: Allocator, elems: *IValuesList) !Self {
        return .{ .elems = try elems.toOwnedSlice(gpa) };
    }

    pub fn get(self: *Self, index: u32) IValue {
        return self.elems[index].*;
    }

    pub fn clone(self: *Self, gpa: Allocator) !Self {
        const new_elems = try gpa.alloc(*IValue, self.elems.len);
        errdefer gpa.free(new_elems);
        for (self.elems, 0..) |elem, i| {
            new_elems[i] = try IValue.init(gpa);
            new_elems[i].* = try elem.clone(gpa);
        }
        return .{ .elems = new_elems };
    }

    pub fn deinit(self: *Self, gpa: Allocator) void {
        for (self.elems) |elem| elem.deinit(gpa);
        gpa.free(self.elems);
    }
};

const HashMap = struct {
    inner: std.HashMap(*IValue, *IValue, Context, 80),
    const Self = @This();

    const Context = struct {
        pub fn hash(self: Context, key: *IValue) u64 {
            _ = self;
            var hasher = std.hash.Wyhash.init(0);
            deepHash(&hasher, key.*);
            return hasher.final();
        }

        pub fn eql(self: Context, a: *IValue, b: *IValue) bool {
            _ = self;
            return deepEqual(a.*, b.*);
        }

        fn deepHash(hasher: anytype, ivalue: IValue) void {
            std.hash.autoHash(hasher, activeTag(ivalue));

            switch (ivalue) {
                .int => |int| std.hash.autoHash(hasher, int),
                .boolean => |boolean| std.hash.autoHash(hasher, boolean),
                .string => |string| hasher.update(string),
                .list => |list| {
                    std.hash.autoHash(hasher, list.elems.len);
                    for (list.elems) |elem_ptr|
                        deepHash(hasher, elem_ptr.*);
                },
                .hash_map => |hash_map| std.hash.autoHash(
                    hasher,
                    hash_map.inner.count(),
                ),
                else => unreachable,
            }
        }
    };

    pub fn init(
        gpa: Allocator,
        keys: *IValuesList,
        values: *IValuesList,
    ) !Self {
        var inner: std.HashMap(*IValue, *IValue, Context, 80) =
            .init(gpa);
        for (keys.items, values.items) |key, value| try inner.put(key, value);
        keys.deinit(gpa);
        values.deinit(gpa);
        return .{ .inner = inner };
    }

    pub fn get(self: *Self, key: *IValue) ?*IValue {
        return self.inner.get(key);
    }

    pub fn put(self: *Self, key: *IValue) !void {
        return self.inner.put(key);
    }

    pub fn clone(self: Self, gpa: Allocator) !Self {
        var new_hash_map: Self = .{ .inner = .init(gpa) };
        errdefer new_hash_map.deinit(gpa);
        try new_hash_map.inner.ensureTotalCapacity(self.inner.count());

        var it = self.inner.iterator();
        while (it.next()) |entry| {
            const old_key_ptr = entry.key_ptr.*;
            const old_value_ptr = entry.value_ptr.*;

            const new_key_ptr = try IValue.init(gpa);
            errdefer gpa.destroy(new_key_ptr);
            new_key_ptr.* = try old_key_ptr.clone(gpa);

            const new_value_ptr = try IValue.init(gpa);
            errdefer new_key_ptr.deinit(gpa);
            errdefer gpa.destroy(new_value_ptr);
            new_value_ptr.* = try old_value_ptr.clone(gpa);

            new_hash_map.inner.putAssumeCapacity(new_key_ptr, new_value_ptr);
        }
        return new_hash_map;
    }

    pub fn deinit(self: *Self, gpa: Allocator) void {
        var it = self.inner.iterator();
        while (it.next()) |entry| {
            const key_ivalue_ptr = entry.key_ptr.*;
            key_ivalue_ptr.clear(gpa);

            const value_ivalue_ptr = entry.key_ptr.*;
            value_ivalue_ptr.clear(gpa);
        }
        self.inner.deinit();
    }
};

fn deepEqual(lhs: IValue, rhs: IValue) bool {
    if (activeTag(lhs) != activeTag(rhs)) return false;
    return switch (lhs) {
        .int => |int| int == rhs.int,
        .boolean => |boolean| boolean == rhs.boolean,
        .string => |string| std.mem.eql(u8, string, rhs.string),
        .list => |list| {
            if (list.elems.len != rhs.list.elems.len) return false;
            for (list.elems, rhs.list.elems) |elem_ptr, rhs_elem_ptr|
                if (!deepEqual(elem_ptr.*, rhs_elem_ptr.*)) return false;
            return true;
        },
        .hash_map => |hash_map| {
            if (hash_map.inner.count() != rhs.hash_map.inner.count())
                return false;
            var it = hash_map.inner.iterator();
            var rhs_it = rhs.hash_map.inner.iterator();
            while (it.next()) |pair| {
                const rhs_pair = rhs_it.next().?;
                const pair_key, const rhs_pair_key =
                    .{ pair.key_ptr.*, rhs_pair.key_ptr.* };
                if (!deepEqual(pair_key.*, rhs_pair_key.*)) return false;
            }
            return true;
        },
        else => unreachable,
    };
}

pub fn visitNode(i: *Interpreter, index: Index) anyerror!IValue {
    const node: Node = i.tree.nodes[@intCast(index)];
    const res: IValue = switch (node) {
        .int => |int| .{ .int = int },
        .string => |string| .{ .string = string },
        // .string => |string| .{ .string = try i.gpa.dupe(u8, string) },
        .boolean => |boolean| .{ .boolean = boolean },
        .ident => |ident| return (try i.getVar(ident)).*,
        .list => |list| .{ .list = try i.listLiteral(list) },
        .hash_map => |hash_map| {
            return .{ .hash_map = try i.hashMapLiteral(hash_map) };
        },

        .bin_expr => |bin_expr| try i.binExpr(bin_expr),
        .cond_expr => |cond_expr| try i.condExpr(cond_expr),
        .index_expr => |index_expr| try i.indexExpr(index_expr),

        .assign_stmt => |assign_stmt| try i.assignStmt(assign_stmt),
        .fn_def => |fn_def| try i.fnDef(index, fn_def),
        .return_stmt => |return_stmt| try i.returnStmt(return_stmt),
        .fn_call => |fn_call| try i.fnCall(fn_call),
        .for_stmt => |for_stmt| try i.forStmt(for_stmt),
        else => return error.UnsupportedNodeType,
    };
    return res;
}

fn listLiteral(i: *Interpreter, list: ast.List) !List {
    const len = list.elems.len;
    var elems = try i.gpa.alloc(*IValue, len);
    errdefer {
        i.gpa.free(elems);
    }
    for (0.., list.elems) |j, elem| {
        elems[j] = try .init(i.gpa);
        elems[j].* = try i.visitNode(elem);
    }
    return .{ .elems = elems };
}

fn hashMapLiteral(i: *Interpreter, hash_map: ast.HashMap) !HashMap {
    const len = hash_map.keys.len;
    var keys = try IValuesList.initCapacity(i.gpa, len);
    var values = try IValuesList.initCapacity(i.gpa, len);
    errdefer {
        for (keys.items) |item| item.deinit(i.gpa);
        keys.deinit(i.gpa);

        for (values.items) |item| item.deinit(i.gpa);
        values.deinit(i.gpa);
    }
    for (hash_map.keys, hash_map.values) |key, value| {
        const key_visited_ptr = try IValue.init(i.gpa);
        key_visited_ptr.* = try i.visitNode(key);
        keys.appendAssumeCapacity(@constCast(key_visited_ptr));

        const value_visited_ptr = try IValue.init(i.gpa);
        value_visited_ptr.* = try i.visitNode(value);
        values.appendAssumeCapacity(@constCast(value_visited_ptr));
    }
    return .init(i.gpa, &keys, &values);
}

fn binExpr(i: *Interpreter, node: ast.BinExpr) anyerror!IValue {
    var lhs = try i.visitNode(node.lhs);
    defer lhs.clear(i.gpa);

    var rhs = try i.visitNode(node.rhs);
    defer rhs.clear(i.gpa);

    const f = &switch (node.op) {
        .add => return add(i.gpa, lhs, rhs),

        .subtr => subtr,
        .mult => mult,
        .power => power,
        .div => div,

        .equal => equal,
        .not_equal => notEqual,
        .greater_than => greaterThan,
        .greater_or_equal_than => greaterOrEqualThan,
        .less_than => lessThan,
        .less_or_equal_than => lessOrEqualThan,

        .logic_or => logicOr,
        .logic_and => logicAnd,
        .is_in => isIn,
    };

    return f(lhs, rhs);
}

fn add(gpa: Allocator, lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    const res: IValue = switch (activeTag(lhs)) {
        .int => .{ .int = lhs.int + rhs.int },
        .string => .{ .string = try std.mem.concat(
            gpa,
            u8,
            &.{ lhs.string, rhs.string },
        ) },
        .list => .{ .list = .{ .elems = try std.mem.concat(
            gpa,
            *IValue,
            &.{ lhs.list.elems, rhs.list.elems },
        ) } },
        else => return error.UnsupportedType,
    };
    return res;
}

fn subtr(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    const res: IValue = switch (activeTag(lhs)) {
        .int => .{ .int = lhs.int - rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn mult(lhs: IValue, rhs: IValue) !IValue {
    const res: IValue = switch (activeTag(lhs)) {
        .int => .{ .int = lhs.int * rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn div(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    const res: IValue = switch (activeTag(lhs)) {
        .int => .{ .int = @divFloor(lhs.int, rhs.int) },
        else => return error.UnsupportedType,
    };
    return res;
}

fn power(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    const res: IValue = switch (activeTag(lhs)) {
        .int => .{ .int = lhs.int ^ rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn notEqual(lhs: IValue, rhs: IValue) !IValue {
    return .{ .boolean = !(try equal(lhs, rhs)).boolean };
}

fn lessThan(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    return switch (activeTag(lhs)) {
        .int => .{ .boolean = lhs.int < rhs.int },
        else => error.UnsupportedType,
    };
}

fn lessOrEqualThan(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    return switch (activeTag(lhs)) {
        .int => .{ .boolean = lhs.int <= rhs.int },
        else => error.UnsupportedType,
    };
}

fn greaterThan(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    return switch (activeTag(lhs)) {
        .int => .{ .boolean = lhs.int > rhs.int },
        else => error.UnsupportedType,
    };
}

fn greaterOrEqualThan(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    return switch (activeTag(lhs)) {
        .int => .{ .boolean = lhs.int >= rhs.int },
        else => error.UnsupportedType,
    };
}

fn logicAnd(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    return (if (!isTruthy(lhs)) lhs else rhs);
}

fn logicOr(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    return (if (isTruthy(lhs)) lhs else rhs);
}

fn isIn(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    const result = switch (rhs) {
        .string => |str| {
            if (lhs != .string) return error.TypeError;
            return .{ .boolean = std.mem.indexOf(
                u8,
                str,
                lhs.string,
            ) != null };
        },
        .list => |list| {
            for (list.elems) |elem_ptr| {
                if (deepEqual(lhs, elem_ptr.*)) return .{ .boolean = true };
            }
            return .{ .boolean = false };
        },
        .hash_map => |hash_map| {
            var key = lhs;
            return .{ .boolean = hash_map.inner.contains(&key) };
        },
        else => return error.UnsupportedType,
    };

    return result;
}

fn checkTypeCompat(lhs: IValue, rhs: IValue) !void {
    @branchHint(.cold);
    if (activeTag(lhs) != activeTag(rhs))
        return error.TypeMismatch;
}

fn condExpr(i: *Interpreter, node: ast.CondExpr) !IValue {
    const if_cond_visited = try i.visitNode(node.if_cond);
    return (if (isTruthy(if_cond_visited))
        i.visitNode(node.then)
    else
        i.visitNode(node.else_expr));
}

fn isTruthy(ivalue: IValue) bool {
    return switch (ivalue) {
        .int => |int| int != 0,
        .boolean => |boolean| boolean,
        .string => |string| string.len > 0,
        .list => |list| list.elems.len > 0,
        .hash_map => |hash_map| hash_map.inner.count() > 0,
        else => unreachable,
    };
}

fn indexExpr(i: *Interpreter, node: ast.IndexExpr) !IValue {
    const target: Node = i.tree.nodes[@intCast(node.target)];
    switch (target) {
        .hash_map => |hash_map| {
            const key = try i.visitNode(node.index);
            for (0..hash_map.keys.len) |j| {
                const index = try i.visitNode(hash_map.keys[j]);
                const keys_match =
                    try equal(key, index);
                if (keys_match.boolean)
                    return i.visitNode(hash_map.values[j]);
            }
            return error.NoSuchKey;
        },
        .list => |list| {
            const index = try i.visitNode(node.index);
            if (index.int >= list.elems.len) return error.IndexOutOfBounds;
            return i.visitNode(list.elems[@intCast(index.int)]);
        },
        else => return error.UnsupportedType,
    }
}

fn equal(lhs: IValue, rhs: IValue) !IValue {
    return .{ .boolean = deepEqual(lhs, rhs) };
}

fn assignStmt(i: *Interpreter, assign_stmt: ast.AssignStmt) !IValue {
    const ivalue = try i.visitNode(assign_stmt.value);
    try i.setVar(assign_stmt.name, ivalue);
    return ivalue;
}

fn fnDef(i: *Interpreter, index: Index, fn_def: ast.FnDef) !IValue {
    const function: IValue = .{ .function = index };
    try i.setVar(fn_def.name, function);
    return .none;
}

fn returnStmt(i: *Interpreter, return_stmt: ast.ReturnStmt) !IValue {
    i.return_value = try i.visitNode(return_stmt.value);
    return error.ReturnTrigger;
}

fn fnCall(i: *Interpreter, fn_call: ast.FnCall) !IValue {
    const fn_ptr = i.getVar(fn_call.name) catch |err| {
        std.debug.print(
            "Runtime Error: Function '{s}' not found\n",
            .{fn_call.name},
        );
        return err;
    };
    const function = fn_ptr.*;
    var evaled_args = try std.ArrayList(*IValue).initCapacity(
        i.gpa,
        fn_call.args_len,
    );
    defer {
        for (evaled_args.items) |item| item.deinit(i.gpa);
        evaled_args.deinit(i.gpa);
    }

    for (0..fn_call.args_len) |offset| {
        const arg_node_index =
            i.tree.adpb[@intCast(fn_call.args_start + offset)];

        const ivalue_ptr = try IValue.init(i.gpa);
        errdefer ivalue_ptr.deinit(i.gpa);
        ivalue_ptr.* = try i.visitNode(arg_node_index);
        evaled_args.appendAssumeCapacity(ivalue_ptr);
    }

    return switch (function) {
        .builtin => |builtin| try builtin(i, evaled_args.items),
        .function => |fn_index| {
            const fn_def = i.tree.nodes[fn_index].fn_def;

            const caller_local = i.local;
            i.local = Table.init(i.gpa);

            defer {
                if (i.local.count() > 0) {
                    var local_it = i.local.valueIterator();
                    while (local_it.next()) |value_ptr| {
                        const ivalue_ptr = value_ptr.*;
                        ivalue_ptr.deinit(i.gpa);
                    }
                }
                i.local.deinit();
                i.local = caller_local;
            }

            if (evaled_args.items.len != fn_def.args_len)
                return error.ArgumentCountMismatch;

            for (0..fn_def.args_len) |offset| {
                const param_node_index =
                    i.tree.adpb[fn_def.args_start + offset];
                const param_name = i.tree.nodes[param_node_index].ident;
                try i.setVar(param_name, evaled_args.items[offset].*);
            }

            return i.execBlock(fn_def.body_start, fn_def.body_len);
        },
        else => error.NotCallable,
    };
}

fn forStmt(i: *Interpreter, for_stmt: ast.ForStmt) !IValue {
    const iterable = try i.visitNode(for_stmt.iterable);
    const variable = for_stmt.variable;
    std.debug.print("For it: {any}\n", .{iterable});
    const start, const len = .{ for_stmt.body_start, for_stmt.body_len };

    switch (iterable) {
        .list => |list| {
            std.debug.print("List len: {d}\n", .{list.elems.len});
            for (list.elems) |elem_ptr| {
                try i.setVar(variable, elem_ptr.*);
                _ = try i.execBlock(start, len);
            }
        },
        .string => |string| {
            for (0..string.len) |j| {
                try i.setVar(variable, .{ .string = string[j .. j + 1] });
                _ = try i.execBlock(start, len);
            }
        },
        .hash_map => |hash_map| {
            var it = hash_map.inner.keyIterator();
            while (it.next()) |key_ptr| {
                const ivalue_ptr = key_ptr.*;
                try i.setVar(variable, ivalue_ptr.*);
                _ = try i.execBlock(start, len);
            }
        },
        .int, .boolean, .none, .function, .builtin => return error.TypeError,
    }

    return .none;
}

fn execBlock(i: *Interpreter, start: Index, len: Index) !IValue {
    for (0..len) |offset| {
        const stmt_node_index = i.tree.adpb[start + offset];

        _ = i.visitNode(stmt_node_index) catch |err| switch (err) {
            error.ReturnTrigger => {
                const ivalue = i.return_value;
                i.return_value = .none;
                return ivalue;
            },
            else => return err,
        };
    }

    return .none;
}

const Table = std.StringHashMap(*IValue);

pub fn getVar(i: *Interpreter, name: []const u8) !*IValue {
    if (i.local.get(name)) |ivalue_ptr|
        return ivalue_ptr
    else if (i.global.get(name)) |ivalue_ptr| return ivalue_ptr;
    std.debug.print("Could not find: {s}\n", .{name});
    return error.NoDefinitionFound;
}

pub fn setVar(i: *Interpreter, name: []const u8, ivalue: IValue) !void {
    const entry = try i.local.getOrPut(name);

    if (entry.found_existing) {
        const ivalue_ptr = entry.value_ptr.*;
        ivalue_ptr.clear(i.gpa);
    } else entry.value_ptr.* = try IValue.init(i.gpa);

    const ivalue_ptr = entry.value_ptr.*;
    ivalue_ptr.* = ivalue;
}

fn builtinPrint(i: *Interpreter, args: []*IValue) anyerror!IValue {
    _ = i;

    for (args, 0..) |arg, j| {
        switch (arg.*) {
            .int => |int| std.debug.print("{d}", .{int}),
            .string => |string| std.debug.print("{s}", .{string}),
            .boolean => |boolean| std.debug.print("{}", .{boolean}),
            .list => |list| std.debug.print(
                "list(len={d})",
                .{list.elems.len},
            ),
            .hash_map => |hash_map| std.debug.print(
                "map(len={d})",
                .{hash_map.inner.count()},
            ),
            else => std.debug.print("unknown", .{}),
        }
        if (j < args.len - 1) std.debug.print(" ", .{});
    }
    std.debug.print("\n", .{});
    return .{ .boolean = true };
}

const std = @import("std");
const activeTag = std.meta.activeTag;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const BinOp = ast.BinOp;
const Index = ast.Index;
const Node = ast.Node;
const Tree = ast.Tree;
