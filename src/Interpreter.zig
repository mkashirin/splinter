/// Arena is required to avoid the unnecessary complexity of the MMM here.
arena: Allocator,
/// Writer interface, which would be used to stream the output.
writer: *std.Io.Writer,
/// Tree usually gets deinitilized by the `defer` statement inside of `main()`.
tree: ast.Tree,

global: Table,
local: Table,
return_value: IValue = .none,
last_ivalue: IValue = .none,
diagnostic: ?Diagnostic = null,
line: usize = 1,
const Interpreter = @This();

pub const Diagnostic = struct { at: usize, description: []const u8 };

pub const Error = Allocator.Error || error{ ReturnTrigger, EvaluationFailed };

pub fn init(
    arena: Allocator,
    writer: *std.Io.Writer,
    tree: ast.Tree,
) !Interpreter {
    var i: Interpreter = .{
        .arena = arena,
        .writer = writer,
        .tree = tree,
        .global = .init(arena),
        .local = .init(arena),
    };

    const builtins: [5]struct { []const u8, IFunc } = .{
        .{ "Print", builtinPrint },
        .{ "Tokens", builtinTokens },
        .{ "Indices", builtinIndices },
        .{ "Select", builtinSelect },
        .{ "Aggregate", builtinAggregate },
    };
    for (builtins) |item| {
        const name, const ifunc = item;
        try i.regBuiltin(name, ifunc);
    }

    return i;
}

fn regBuiltin(i: *Interpreter, name: []const u8, ifunc: IFunc) !void {
    const ifunc_: *IValue = try .create(i.arena);
    ifunc_.* = .{ .ifunc = ifunc };
    try i.global.put(name, ifunc_);
}

pub fn deinit(i: *Interpreter) void {
    if (i.global.count() > 0) {
        var global_it = i.global.valueIterator();
        while (global_it.next()) |value_ptr| {
            const ivalue_ptr = value_ptr.*;
            ivalue_ptr.clearAndDestroy(i.arena);
        }
    }
    i.global.deinit();

    if (i.local.count() > 0) {
        var local_it = i.local.valueIterator();
        while (local_it.next()) |value_ptr| {
            const ivalue_ptr = value_ptr.*;
            ivalue_ptr.clearAndDestroy(i.arena);
        }
    }
    i.local.deinit();
}

pub fn walkTree(i: *Interpreter) !IValue {
    for (i.tree.indices) |stmt_index| {
        i.last_ivalue.clear();

        i.last_ivalue = i.visitNode(stmt_index) catch |err| switch (err) {
            error.ReturnTrigger => {
                const ivalue = i.return_value;
                i.return_value = .none;
                return ivalue;
            },
            else => return err,
        };
        i.line += 1;
    }

    return i.last_ivalue;
}

fn fail(i: *Interpreter, reason: []const u8) Error {
    i.diagnostic = .{ .at = i.line, .description = reason };
    return Error.EvaluationFailed;
}

pub const IValue = union(enum) {
    int: i64,
    string: []const u8,
    boolean: bool,
    list: List,
    hash_map: HashMap,
    none: void,
    ifunc: IFunc,
    fn_index: u32,
    imatrix: IMatrix,
    lazy_bin: LazyBin,

    // TODO: Figure out, how to NOT store this as a massive IValue object,
    // passing it as a simple enum member instead.
    op_arg: ast.BinOp,

    const Self = @This();

    pub fn create(gpa: Allocator) !*Self {
        return gpa.create(Self);
    }

    pub fn clearAndDestroy(self: *Self, gpa: Allocator) void {
        self.clear();
        gpa.destroy(self);
    }

    pub fn clear(self: *Self) void {
        self.* = .none;
    }

    pub fn clone(self: *Self, gpa: Allocator) !Self {
        return switch (self.*) {
            .int => |int| .{ .int = int },
            .string => |string| .{ .string = try gpa.dupe(u8, string) },
            .boolean => |boolean| .{ .boolean = boolean },
            .list => |*list| .{ .list = try list.clone(gpa) },
            .hash_map => |*hash_map| .{ .hash_map = try hash_map.clone() },
            else => self.*,
        };
    }

    pub fn isCallable(self: *Self) bool {
        return switch (self.*) {
            .ifunc, .fn_index, .lazy_bin => true,
            else => false,
        };
    }
};

const List = struct {
    elems: []const *IValue,
    const Self = @This();

    pub fn clear(self: *Self, gpa: Allocator) void {
        for (self.elems) |elem| elem.clearAndDestroy(gpa);
    }

    pub fn get(self: *Self, index: u32) IValue {
        return self.elems[index].*;
    }

    pub fn set(self: *Self, index: u32, value: *IValue) !void {
        self.elems[index] = value;
    }

    pub fn clone(self: *Self, gpa: Allocator) !Self {
        const len = self.elems.len;
        var elems = try gpa.alloc(*IValue, len);
        for (0.., self.elems) |i, elem| {
            elems[i] = try .create(gpa);
            elems[i].* = elem.*;
        }
        return .{ .elems = elems };
    }
};

const HashMap = struct {
    inner: std.HashMap(IValue, IValue, Context, 80),
    const Self = @This();

    const Context = struct {
        pub fn hash(self: Context, key: IValue) u64 {
            _ = self;
            var hasher = std.hash.Wyhash.init(0);
            deepHash(&hasher, key);
            return hasher.final();
        }

        pub fn eql(self: Context, a: IValue, b: IValue) bool {
            _ = self;
            return deepEqual(a, b);
        }

        fn deepHash(hasher: anytype, ivalue: IValue) void {
            std.hash.autoHash(hasher, meta.activeTag(ivalue));

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

    pub fn create(gpa: Allocator, keys: []*IValue, values: []*IValue) !Self {
        var inner: std.HashMap(IValue, IValue, Context, 80) = .init(gpa);
        for (keys, values) |key, value| try inner.put(key.*, value.*);
        return .{ .inner = inner };
    }

    pub fn clear(self: *Self) void {
        self.inner.clearAndFree();
        self.* = .none;
    }

    pub fn get(self: *Self, key: IValue) ?IValue {
        return self.inner.get(key);
    }

    pub fn put(self: *Self, key: IValue) !void {
        return self.inner.put(key);
    }

    pub fn clone(self: *Self) !Self {
        return .{ .inner = try self.inner.clone() };
    }
};

const IFunc = *const fn (*Interpreter, []*IValue) Error!IValue;

fn deepEqual(lhs: IValue, rhs: IValue) bool {
    if (meta.activeTag(lhs) != meta.activeTag(rhs)) return false;
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
                if (!deepEqual(pair_key, rhs_pair_key)) return false;
            }
            return true;
        },
        else => unreachable,
    };
}

const IMatrix = struct {
    gpa: Allocator,
    rows: u32,
    columns: u32,
    data: []*IValue,
    const Self = @This();

    pub fn init(gpa: Allocator, rows: u32, columns: u32) !Self {
        const data = try gpa.alloc(*IValue, rows * columns);
        const none: *IValue = try .create(gpa);
        none.* = .none;
        @memset(data, none);

        return .{
            .gpa = gpa,
            .rows = rows,
            .columns = columns,
            .data = data,
        };
    }

    pub fn deinit(self: Self) void {
        self.gpa.free(self.data);
    }

    inline fn index(self: Self, r: u32, c: u32) usize {
        std.debug.assert(r < self.rows and c < self.columns);
        return r * self.columns + c;
    }

    pub fn get(self: Self, r: u32, c: u32) *IValue {
        return self.data[self.index(r, c)];
    }

    pub fn set(self: *Self, r: u32, c: u32, value: *IValue) void {
        self.data[self.index(r, c)] = value;
    }
};

const LazyBin = struct { lhs: *IValue, op: ast.BinOp, rhs: *IValue };

pub fn visitNode(i: *Interpreter, index: ast.Index) Error!IValue {
    const node: ast.Node = i.tree.nodes[@intCast(index)];
    const res: IValue = switch (node) {
        .int => |int| .{ .int = int },
        .string => |string| .{ .string = string },
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
        .call => |call_| try i.call(call_),
        .op_arg => |bin_op| try i.opArg(bin_op),
        .for_stmt => |for_stmt| try i.forStmt(for_stmt),
        else => return i.fail("Visited invalid node type"),
    };
    return res;
}

fn listLiteral(i: *Interpreter, list: ast.List) !List {
    const len = list.elems.len;
    var elems = try i.arena.alloc(*IValue, len);
    for (0.., list.elems) |j, elem| {
        elems[j] = try .create(i.arena);
        elems[j].* = try i.visitNode(elem);
    }
    return .{ .elems = elems };
}

fn hashMapLiteral(i: *Interpreter, hash_map: ast.HashMap) !HashMap {
    const len = hash_map.keys.len;
    var keys = try i.arena.alloc(*IValue, len);
    var values = try i.arena.alloc(*IValue, len);
    for (0.., hash_map.keys, hash_map.values) |j, key, value| {
        keys[j] = try .create(i.arena);
        keys[j].* = try i.visitNode(key);

        values[j] = try .create(i.arena);
        values[j].* = try i.visitNode(value);
    }
    return .create(i.arena, keys, values);
}

fn binExpr(i: *Interpreter, node: ast.BinExpr) Error!IValue {
    var lhs = try i.visitNode(node.lhs);
    defer lhs.clear();

    var rhs = try i.visitNode(node.rhs);
    defer rhs.clear();

    const f = &switch (node.op) {
        .add => add,
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

    return f(i, lhs, rhs);
}

fn add(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    return switch (lhs) {
        .int => .{ .int = lhs.int + rhs.int },
        .string => .{ .string = try std.mem.concat(
            i.arena,
            u8,
            &.{ lhs.string, rhs.string },
        ) },
        .list => .{ .list = .{ .elems = try std.mem.concat(
            i.arena,
            *IValue,
            &.{ lhs.list.elems, rhs.list.elems },
        ) } },
        else => i.fail("Invalid type for operation: ADD"),
    };
}

fn subtr(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    return switch (lhs) {
        .int => .{ .int = lhs.int - rhs.int },
        else => i.fail("Invalid type for operation: SUB"),
    };
}

fn mult(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    return switch (lhs) {
        .int => .{ .int = lhs.int * rhs.int },
        else => i.fail("Invalid type for operation: MULT"),
    };
}

fn div(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    return switch (lhs) {
        .int => .{ .int = @divFloor(lhs.int, rhs.int) },
        else => i.fail("Invalid type for operation: DIV"),
    };
}

fn power(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    return switch (lhs) {
        .int => .{ .int = std.math.pow(i64, lhs.int, rhs.int) },
        else => i.fail("Invalid type for operation: POW"),
    };
}

fn notEqual(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    return .{ .boolean = !(try i.equal(lhs, rhs)).boolean };
}

fn lessThan(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    return switch (lhs) {
        .int => .{ .boolean = lhs.int < rhs.int },
        else => i.fail("Invalid type for operation: LT"),
    };
}

fn lessOrEqualThan(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    return switch (lhs) {
        .int => .{ .boolean = lhs.int <= rhs.int },
        else => i.fail("Invalid type for operation: LET"),
    };
}

fn greaterThan(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    return switch (lhs) {
        .int => .{ .boolean = lhs.int > rhs.int },
        else => i.fail("Invalid type for operation: GT"),
    };
}

fn greaterOrEqualThan(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    return switch (lhs) {
        .int => .{ .boolean = lhs.int >= rhs.int },
        else => i.fail("Invalid type for operation: GET"),
    };
}

fn logicAnd(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    _ = i;
    return (if (!isTruthy(lhs)) lhs else rhs);
}

fn logicOr(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    _ = i;
    return (if (isTruthy(lhs)) lhs else rhs);
}

fn isIn(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    switch (rhs) {
        .string => |str| {
            if (lhs != .string) return i.fail("Both sides must be strings");
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
            const key = lhs;
            return .{ .boolean = hash_map.inner.contains(key) };
        },
        else => return i.fail("Invalid type for operation: ISIN"),
    }
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
    var target = try i.visitNode(node.target);
    const index = try i.visitNode(node.index);
    // TODO: Support indexing into IMatrix.
    switch (target) {
        .hash_map => |*hash_map| return hash_map.get(index) orelse .none,
        .list => |*list| {
            if (index.int >= list.elems.len)
                return i.fail("List index out of bounds");
            return list.get(@intCast(index.int));
        },
        else => return i.fail("Could not index into the type"),
    }
}

fn equal(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    _ = i;
    return .{ .boolean = deepEqual(lhs, rhs) };
}

fn assignStmt(i: *Interpreter, assign_stmt: ast.AssignStmt) !IValue {
    const ivalue = try i.visitNode(assign_stmt.value);
    try i.setVar(assign_stmt.name, ivalue);
    return ivalue;
}

fn fnDef(i: *Interpreter, index: ast.Index, fn_def: ast.FnDef) !IValue {
    const function: IValue = .{ .fn_index = index };
    try i.setVar(fn_def.name, function);
    return .none;
}

fn returnStmt(i: *Interpreter, return_stmt: ast.ReturnStmt) !IValue {
    i.return_value = try i.visitNode(return_stmt.value);
    return Error.ReturnTrigger;
}

// TODO: Implement lazy evaluation, to enable support for advanced call syntax.
fn call(i: *Interpreter, call_: ast.Call) !IValue {
    switch (call_.callable) {
        .ident => return i.callIdent(call_),
        .expr => |expr| {
            const res = try i.visitNode(expr);
            std.debug.print("call res: {any}", .{res});
            return res;
        },
    }
}

fn callIdent(i: *Interpreter, call_: ast.Call) !IValue {
    const fn_ptr = try i.getVar(call_.callable.ident);
    const function = fn_ptr.*;
    var evaled_args = try i.arena.alloc(*IValue, call_.args_len);
    for (0..call_.args_len) |offset| {
        const arg_node_index =
            i.tree.adpb[@intCast(call_.args_start + offset)];

        evaled_args[offset] = try IValue.create(i.arena);
        evaled_args[offset].* = try i.visitNode(arg_node_index);
    }

    return switch (function) {
        .ifunc => |ifunc| try ifunc(i, evaled_args),
        .fn_index => |fn_index| {
            const fn_def = i.tree.nodes[fn_index].fn_def;

            const caller_local = i.local;
            i.local = Table.init(i.arena);
            defer i.local = caller_local;

            if (evaled_args.len != fn_def.args_len)
                return i.fail("Argument count mismatch");

            for (0..fn_def.args_len) |offset| {
                const param_node_index =
                    i.tree.adpb[fn_def.args_start + offset];
                const param_name = i.tree.nodes[param_node_index].ident;
                try i.setVar(param_name, evaled_args[offset].*);
            }

            return i.execBlock(fn_def.body_start, fn_def.body_len);
        },
        else => i.fail("IValue is not callable"),
    };
}

fn opArg(i: *Interpreter, bin_op: ast.BinOp) !IValue {
    return switch (bin_op) {
        .equal,
        .not_equal,
        .greater_than,
        .greater_or_equal_than,
        .less_than,
        .less_or_equal_than,
        => .{ .op_arg = bin_op },
        else => i.fail("Invalid operational argument"),
    };
}

fn forStmt(i: *Interpreter, for_stmt: ast.ForStmt) !IValue {
    const iterable = try i.visitNode(for_stmt.iterable);
    const variable = for_stmt.variable;
    const start, const len = .{ for_stmt.body_start, for_stmt.body_len };

    switch (iterable) {
        .list => |list| {
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
                try i.setVar(variable, key_ptr.*);
                _ = try i.execBlock(start, len);
            }
        },
        else => return i.fail("IValue is not iterable"),
    }

    return .none;
}

fn execBlock(i: *Interpreter, start: ast.Index, len: ast.Index) !IValue {
    for (0..len) |offset| {
        const stmt_node_index = i.tree.adpb[start + offset];

        _ = i.visitNode(stmt_node_index) catch |err| switch (err) {
            Error.ReturnTrigger => {
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
    return i.fail("No definition found");
}

pub fn setVar(i: *Interpreter, name: []const u8, ivalue: IValue) !void {
    const entry = try i.local.getOrPut(name);

    if (entry.found_existing) {
        const ivalue_ptr = entry.value_ptr.*;
        ivalue_ptr.clear();
    } else entry.value_ptr.* = try IValue.create(i.arena);

    const ivalue_ptr = entry.value_ptr.*;
    ivalue_ptr.* = ivalue;
}

fn builtinPrint(i: *Interpreter, args: []*IValue) !IValue {
    for (args, 0..) |arg, j| {
        switch (arg.*) {
            .int => |int| i.iprint("{d}", .{int}),
            .string => |string| i.iprint("{s}", .{string}),
            .boolean => |boolean| i.iprint("{}", .{boolean}),
            .list => |list| i.iprint(
                "List(len={d})",
                .{list.elems.len},
            ),
            .hash_map => |hash_map| i.iprint(
                "Map(len={d})",
                .{hash_map.inner.count()},
            ),
            .imatrix => |imatrix| i.iprint(
                "IMatrix({d}x{d})",
                .{ imatrix.rows, imatrix.columns },
            ),
            else => i.iprint("IValue at {*}", .{arg}),
        }
        if (j < args.len - 1) i.iprint(" ", .{});
    }
    i.iprint("\n", .{});
    return .none;
}

/// Infallible print.
fn iprint(
    i: *Interpreter,
    comptime format: []const u8,
    args: anytype,
) void {
    i.writer.print(format, args) catch unreachable;
}

fn builtinTokens(i: *Interpreter, args: []*IValue) !IValue {
    if (args.len != 1) return i.fail("Expected only 1 string as an argument");

    const string: []const u8 = args[0].string;
    const elems = try i.arena.alloc(*IValue, string.len);
    for (0.., string) |j, character| {
        var token: [1]u8 = undefined;
        token[0] = character;
        elems[j] = try .create(i.arena);
        elems[j].* = .{ .string = try i.arena.dupe(u8, &token) };
    }
    const list: List = .{ .elems = elems };
    return .{ .list = list };
}

fn builtinIndices(i: *Interpreter, args: []*IValue) !IValue {
    if (args.len != 1) return i.fail("Expected only 1 string as an argument");

    const string: []const u8 = args[0].string;
    const elems = try i.arena.alloc(*IValue, string.len);
    for (0..string.len) |j| {
        elems[j] = try .create(i.arena);
        elems[j].* = .{ .int = @intCast(j) };
    }
    const list: List = .{ .elems = elems };
    return .{ .list = list };
}

fn builtinSelect(i: *Interpreter, args: []*IValue) !IValue {
    if (args.len != 3)
        return i.fail("Expected exactly 3 arguments: 2 collections and 1 operational argument");

    const lhs: List = args[0].list;
    const op_arg: ast.BinOp = args[2].op_arg;
    const rhs: List = args[1].list;
    var imatrix: IMatrix =
        try .init(i.arena, @intCast(rhs.elems.len), @intCast(lhs.elems.len));

    for (0.., rhs.elems) |row, rhs_elem| {
        for (0.., lhs.elems) |column, lhs_elem| {
            const f = &switch (op_arg) {
                .equal => equal,
                .not_equal => notEqual,
                .greater_than => greaterThan,
                .greater_or_equal_than => greaterOrEqualThan,
                .less_than => lessThan,
                .less_or_equal_than => lessOrEqualThan,
                else => unreachable,
            };
            const imatrix_elem: *IValue = try .create(i.arena);
            imatrix_elem.* = try f(i, lhs_elem.*, rhs_elem.*);
            imatrix.set(@intCast(row), @intCast(column), imatrix_elem);
        }
    }
    return .{ .imatrix = imatrix };
}

fn builtinAggregate(i: *Interpreter, args: []*IValue) Error!IValue {
    if (args.len != 2)
        return i.fail("Exactly 2 arguments expected: selector matrix and a collection");

    const selector = args[0].imatrix;
    const in = args[1].list;
    var elems = try i.arena.alloc(*IValue, in.elems.len);
    for (0..selector.rows) |row| {
        var acc: i64, var agg: u32 = .{ 0, 0 };
        for (0.., in.elems) |column, in_elem| {
            if (selector.get(@intCast(row), @intCast(column)).boolean) {
                acc += in_elem.int;
                agg += 1;
            }
        }
        const elem = if (agg > 0) @divFloor(acc, agg) else 0;
        elems[row] = try .create(i.arena);
        elems[row].* = .{ .int = elem };
    }
    const list: List = .{ .elems = elems };
    return .{ .list = list };
}

const std = @import("std");
const meta = std.meta;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
