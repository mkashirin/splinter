/// Arena is required to avoid the unnecessary complexity of the MMM here.
arena: Allocator,
/// Writer interface, which would be used to stream the output.
writer: *std.Io.Writer,
/// Tree usually gets deinitilized by the `defer` statement inside of `main()`.
tree: ast.Tree,

global: Table,
local: Table,
return_value: IValue = .none,
last_value: IValue = .none,
diagnostic: ?Diagnostic = null,
line: usize = 1,

// TODO: Implement call chaining and functions as arguments.
const Interpreter = @This();

const Table = std.StringHashMap(IValue);

pub fn getVar(i: *Interpreter, name: []const u8) !IValue {
    // var it = i.global.keyIterator();
    // while (it.next()) |key| std.debug.print("Entry {s}\n", .{key.*});
    if (i.local.get(name)) |value| {
        return value;
    } else if (i.global.get(name)) |value| {
        return value;
    } else {
        var _reason: [48]u8 = undefined;
        var writer = std.Io.Writer.fixed(&_reason);
        _ = writer.print("No definition found for symbol '{s}'", .{name}) catch unreachable;
        return i.fail(&_reason, error.UndefinedSymbol);
    }
}

pub fn setVar(
    i: *Interpreter,
    table: enum { global, local },
    name: []const u8,
    value: IValue,
) !void {
    var table_ = if (table == .global) &i.global else &i.local;
    const entry = try table_.getOrPut(name);
    if (entry.found_existing) {
        var value_ = entry.value_ptr.*;
        value_.clear();
    } else entry.value_ptr.* = value;
    entry.value_ptr.* = value;
}

pub const Diagnostic = struct { at: usize, description: []const u8 };

pub const Error = Allocator.Error || error{
    EvaluationFailed,
    UndefinedSymbol,
    ReturnTrigger,
    TypeError,
    BuiltinFailed,
};

pub fn init(arena: Allocator, writer: *std.Io.Writer, tree: ast.Tree) !Interpreter {
    var i: Interpreter = .{
        .arena = arena,
        .writer = writer,
        .tree = tree,
        .global = .init(arena),
        .local = .init(arena),
    };

    const builtins: [6]struct { []const u8, IFunc } = .{
        .{ "Print", builtinPrint },
        .{ "Tokens", builtinTokens },
        .{ "Indicator", builtinIndicator },
        .{ "Indices", builtinIndices },
        .{ "Select", builtinSelect },
        .{ "Aggregate", builtinAggregate },
    };
    for (builtins) |item| {
        const name, const func = item;
        try i.regBuiltin(name, func);
    }

    return i;
}

inline fn getNode(i: *Interpreter, index: ast.Index) ast.Node {
    return i.tree.nodes[@intCast(index)];
}

fn regBuiltin(i: *Interpreter, name: []const u8, func: IFunc) !void {
    try i.global.put(name, .{ .func = func });
}

pub fn deinit(i: *Interpreter) void {
    if (i.global.count() > 0) {
        var global_it = i.global.valueIterator();
        while (global_it.next()) |value_ptr| value_ptr.clearAndDestroy(i.arena);
    }
    i.global.deinit();

    if (i.local.count() > 0) {
        var local_it = i.local.valueIterator();
        while (local_it.next()) |value_ptr| value_ptr.clearAndDestroy(i.arena);
    }
    i.local.deinit();
}

pub fn walkTree(i: *Interpreter) !IValue {
    for (i.tree.indices) |stmt_index| {
        i.last_value.clear();

        i.last_value = i.visit(stmt_index) catch |err| switch (err) {
            error.ReturnTrigger => {
                const value = i.return_value;
                i.return_value = .none;
                return value;
            },
            else => return err,
        };
        i.line += 1;
    }

    return i.last_value;
}

fn fail(i: *Interpreter, reason: []const u8, err: ?Error) Error {
    @branchHint(.cold);
    const err_ = err orelse error.EvaluationFailed;
    i.diagnostic = .{ .at = i.line, .description = reason };
    return err_;
}

// ------------------------------------------------------------------------------------------------
// IValue type and variants
// ------------------------------------------------------------------------------------------------

pub const IValue = union(enum) {
    float: f64,
    int: i32,
    string: []const u8,
    boolean: bool,
    list: *List,
    hash_map: *HashMap,
    none: void,
    func: IFunc,
    fn_index: ast.Index,
    matrix: *IMatrix,
    lazy_index: ast.Index,
    op_arg: ast.BinOp,
    const Self = @This();
    const Tag = meta.Tag(Self);

    pub fn clearAndDestroy(self: *Self, gpa: Allocator) void {
        self.clear();
        gpa.destroy(self);
    }

    pub inline fn clear(self: *Self) void {
        self.* = .none;
    }

    pub inline fn tag(self: Self) Tag {
        return meta.activeTag(self);
    }

    pub inline fn is(self: Self, tag_: Tag) bool {
        return self == tag_;
    }

    pub inline fn isOneOf(self: Self, tags: []const Tag) bool {
        for (tags) |tag_| if (self == tag_) return true;
        return false;
    }

    fn _iprint(w: *std.Io.Writer, comptime format: []const u8, args: anytype) void {
        w.print(format, args) catch unreachable;
    }

    pub fn repr(self: Self, w: *std.Io.Writer) !void {
        switch (self) {
            .float => |float| _iprint(w, "Float({d})", .{float}),
            .int => |int| _iprint(w, "Int({d})", .{int}),
            .string => |string| _iprint(w, "String({s})", .{string}),
            .boolean => |boolean| _iprint(w, "Boolean({})", .{boolean}),
            .list => |list| {
                _iprint(w, "List([", .{});
                const num_elems = self.len().?;
                for (0..num_elems) |i| {
                    try list.get(@intCast(i)).repr(w);
                    if (i < num_elems - 1) _iprint(w, ", ", .{});
                }
                _iprint(w, "])", .{});
            },
            .hash_map => |hash_map| {
                _iprint(w, "Map({{", .{});
                const num_entries = self.len().?;
                var it = hash_map.inner.iterator();
                var i: usize = 0;
                while (it.next()) |entry| : (i += 1) {
                    try entry.key_ptr.repr(w);
                    _iprint(w, ": ", .{});
                    try entry.value_ptr.repr(w);
                    if (i < num_entries - 1) _iprint(w, ", ", .{}) else _iprint(w, "}})", .{});
                }
            },
            .matrix => |matrix| {
                _iprint(w, "Matrix([\n", .{});
                for (0..matrix.rows) |row| {
                    _iprint(w, "    [", .{});
                    for (0..matrix.columns) |column| {
                        const elem = matrix.get(@intCast(row), @intCast(column));
                        _iprint(w, "({d}, {d}): ", .{ row, column });
                        try elem.repr(w);
                        if (column < matrix.columns - 1) {
                            _iprint(w, ", ", .{});
                        } else if (row < matrix.rows) _iprint(w, "],\n", .{});
                    }
                }
                _iprint(w, "])", .{});
            },
            else => unreachable,
        }
    }

    pub inline fn len(self: Self) ?usize {
        return switch (self) {
            .string => |string| string.len,
            .list => |list| list.elems.len,
            .hash_map => |hash_map| hash_map.inner.count(),
            else => null,
        };
    }

    pub fn clone(self: Self, gpa: Allocator) !Self {
        return switch (self.*) {
            .int => |int| .{ .int = int },
            .string => |string| .{ .string = try gpa.dupe(u8, string) },
            .boolean => |boolean| .{ .boolean = boolean },
            .list => |list| .{ .list = try list.clone(gpa) },
            .hash_map => |hash_map| .{ .hash_map = try hash_map.clone() },
            else => self.*,
        };
    }
};

const List = struct {
    elems: []IValue,
    const Self = @This();

    pub fn makePointer(self: Self, gpa: Allocator) !*Self {
        const list_ptr: *Self = try gpa.create(Self);
        list_ptr.* = self;
        return list_ptr;
    }

    pub fn clear(self: *Self, gpa: Allocator) void {
        for (self.elems) |elem| elem.clearAndDestroy(gpa);
    }

    pub inline fn get(self: *Self, index: u32) IValue {
        return self.elems[index];
    }

    pub inline fn set(self: *Self, index: u32, value: *IValue) !void {
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

        fn deepHash(hasher: anytype, value: IValue) void {
            std.hash.autoHash(hasher, value.tag());

            switch (value) {
                .int => |int| std.hash.autoHash(hasher, int),
                .boolean => |boolean| std.hash.autoHash(hasher, boolean),
                .string => |string| hasher.update(string),
                .list => |list| {
                    std.hash.autoHash(hasher, list.elems.len);
                    for (list.elems) |elem| deepHash(hasher, elem);
                },
                .hash_map => |hash_map| std.hash.autoHash(hasher, hash_map.inner.count()),
                else => unreachable,
            }
        }
    };

    pub fn init(gpa: Allocator, keys: []IValue, values: []IValue) !Self {
        var inner: std.HashMap(IValue, IValue, Context, 80) = .init(gpa);
        for (keys, values) |key, value| try inner.put(key, value);
        return .{ .inner = inner };
    }

    pub fn makePointer(self: Self, gpa: Allocator) !*Self {
        const hash_map_ptr: *Self = try gpa.create(Self);
        hash_map_ptr.* = self;
        return hash_map_ptr;
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

const IFunc = *const fn (*Interpreter, []IValue) Error!IValue;
fn deepEqual(lhs: IValue, rhs: IValue) bool {
    if (lhs.is(.float) and rhs.is(.int)) {
        return deepEqual(rhs, lhs);
    } else if (lhs.is(.int) and rhs.is(.float)) {
        return @as(f64, @floatFromInt(lhs.int)) == rhs.float;
    }

    if (!lhs.is(rhs.tag())) return false;
    return switch (lhs) {
        .int => |int| int == rhs.int,
        .float => |float| float == rhs.float,
        .boolean => |boolean| boolean == rhs.boolean,
        .string => |string| std.mem.eql(u8, string, rhs.string),

        .list => |list| eq: {
            if (list.elems.len != rhs.list.elems.len) break :eq false;

            for (list.elems, rhs.list.elems) |elem, rhs_elem| {
                if (!deepEqual(elem, rhs_elem)) break :eq false;
            }

            break :eq true;
        },

        .hash_map => |hash_map| eq: {
            if (hash_map.inner.count() != rhs.hash_map.inner.count())
                break :eq false;

            var it = hash_map.inner.iterator();
            while (it.next()) |pair| {
                var found_match = false;
                var rhs_it = rhs.hash_map.inner.iterator();

                while (rhs_it.next()) |rhs_pair| {
                    if (deepEqual(pair.key_ptr.*, rhs_pair.key_ptr.*)) {
                        if (deepEqual(pair.value_ptr.*, rhs_pair.value_ptr.*)) {
                            found_match = true;
                        }
                        break;
                    }
                }

                if (!found_match) break :eq false;
            }

            break :eq true;
        },

        else => unreachable,
    };
}

// NOTE: For now, this is a type, which only interpreter gets to makes use of.
// User cannot interact with it in any way, except for looking at it's
// representation, which is available via `builtinPrint()`. Bounds check on
// access indices should be implemented properly before exposing the `IMatrix`
// interaction API.
const IMatrix = struct {
    gpa: Allocator,
    rows: u32,
    columns: u32,
    data: []IValue,
    const Self = @This();

    pub fn makePointer(self: Self, gpa: Allocator) !*Self {
        const matrix_ptr: *Self = try gpa.create(Self);
        matrix_ptr.* = self;
        return matrix_ptr;
    }

    pub fn init(gpa: Allocator, rows: u32, columns: u32) !Self {
        const data = try gpa.alloc(IValue, rows * columns);
        const none: IValue = .none;
        @memset(data, none);

        return .{ .gpa = gpa, .rows = rows, .columns = columns, .data = data };
    }

    pub fn deinit(self: Self) void {
        self.gpa.free(self.data);
    }

    // NOTE: Since this is getting used in built-in functions only, the logic
    // of these already implies all the neccessary constraints on the access
    // indices.
    inline fn index(self: Self, r: u32, c: u32) usize {
        return r * self.columns + c;
    }

    pub fn get(self: Self, r: u32, c: u32) IValue {
        return self.data[self.index(r, c)];
    }

    pub fn set(self: *Self, r: u32, c: u32, value: IValue) void {
        self.data[self.index(r, c)] = value;
    }
};

pub fn visit(i: *Interpreter, index: ast.Index) Error!IValue {
    const node: ast.Node = i.getNode(index);
    return switch (node) {
        .float => |float| .{ .float = float },
        .int => |int| .{ .int = int },
        .string => |string| .{ .string = string },
        .boolean => |boolean| .{ .boolean = boolean },
        .ident => |ident| i.getVar(ident),
        .list => |list| .{ .list = try i.listLiteral(list) },
        .list_comp => |list_comp| .{ .list = try i.listComp(list_comp) },
        .hash_map => |hash_map| .{ .hash_map = try i.hashMapLiteral(hash_map) },

        .bin_expr => |bin_expr| i.binExpr(bin_expr),
        .cond_expr => |cond_expr| i.condExpr(cond_expr),
        .index_expr => |index_expr| i.indexExpr(index_expr),

        .assign_stmt => |assign_stmt| i.assignStmt(assign_stmt),
        .fn_def => |fn_def| i.fnDef(index, fn_def),
        .return_stmt => |return_stmt| try i.returnStmt(return_stmt),
        .call => |any_call| i.call(any_call),
        .op_arg => |bin_op| i.opArg(bin_op),
        .for_stmt => |for_stmt| i.forStmt(for_stmt),
    };
}

// ------------------------------------------------------------------------------------------------
// Literals
// ------------------------------------------------------------------------------------------------

fn listLiteral(i: *Interpreter, list: ast.List) !*List {
    const len = list.elems.len;
    var elems = try i.arena.alloc(IValue, len);
    for (0.., list.elems) |j, elem| {
        elems[j] = try i.visit(elem);
    }
    const list_: List = .{ .elems = elems };
    return list_.makePointer(i.arena);
}

fn listComp(i: *Interpreter, list_comp: ast.ListComp) !*List {
    const iter = try i.visit(list_comp.iter);
    const variable = list_comp.variable;
    const elems = try i.arena.alloc(IValue, iter.len().?);
    switch (iter) {
        .string => |string| {
            for (0.., string) |j, char| {
                const buf = [1]u8{char};
                try i.setVar(.local, variable, .{ .string = &buf });
                elems[j] = try i.visit(list_comp.expr);
            }
        },
        .list => |list| {
            for (0.., list.elems) |j, elem| {
                try i.setVar(.local, variable, elem);
                elems[j] = try i.visit(list_comp.expr);
            }
        },
        .hash_map => |hash_map| {
            var it = hash_map.inner.keyIterator();
            var j: usize = 0;
            while (it.next()) |key_ptr| : (j += 1) {
                try i.setVar(.local, variable, key_ptr.*);
                elems[j] = try i.visit(list_comp.expr);
            }
        },
        else => return i.fail("Invalid type for list comprehension iterable", null),
    }
    const list_: List = .{ .elems = elems };
    return list_.makePointer(i.arena);
}

fn hashMapLiteral(i: *Interpreter, hash_map: ast.HashMap) !*HashMap {
    const len = hash_map.keys.len;
    var keys = try i.arena.alloc(IValue, len);
    var values = try i.arena.alloc(IValue, len);
    for (0.., hash_map.keys, hash_map.values) |j, key, value| {
        keys[j] = try i.visit(key);
        values[j] = try i.visit(value);
    }
    const hash_map_: HashMap = try .init(i.arena, keys, values);
    return hash_map_.makePointer(i.arena);
}

// ------------------------------------------------------------------------------------------------
// Binary and conditional expressions: arithmetic, comparisons and logic
// ------------------------------------------------------------------------------------------------

fn binExpr(i: *Interpreter, bin_expr: ast.BinExpr) Error!IValue {
    const lhs, const rhs = .{ try i.visit(bin_expr.lhs), try i.visit(bin_expr.rhs) };
    return i.bin(bin_expr.op, lhs, rhs);
}

fn bin(i: *Interpreter, op: ast.BinOp, lhs: IValue, rhs: IValue) Error!IValue {
    const f = &switch (op) {
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
    if (lhs.isOneOf(&.{ .float, .int }) and rhs.is(.list)) return i.add(rhs, lhs);
    return switch (lhs) {
        .int => .{ .int = lhs.int + rhs.int },
        .float => .{ .float = lhs.float + rhs.float },
        .string => .{ .string = try std.mem.concat(i.arena, u8, &.{ lhs.string, rhs.string }) },
        .list => list: switch (rhs) {
            .int, .string => {
                for (0.., lhs.list.elems) |j, elem| lhs.list.elems[j] = try i.add(elem, rhs);
                break :list lhs;
            },
            .list => {
                const list_: List = .{ .elems = try std.mem.concat(
                    i.arena,
                    IValue,
                    &.{ lhs.list.elems, rhs.list.elems },
                ) };
                break :list .{ .list = try list_.makePointer(i.arena) };
            },
            else => i.fail("Values could not be broadcasted for the list", error.TypeError),
        },
        else => i.fail("Invalid type for addition operation", error.TypeError),
    };
}

fn subtr(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    if (lhs.isOneOf(&.{ .float, .int }) and rhs.is(.list)) return i.subtr(rhs, lhs);

    return switch (lhs) {
        .int => switch (rhs) {
            .int => .{ .int = lhs.int - rhs.int },
            .float => .{ .float = @as(f64, @floatFromInt(lhs.int)) - rhs.float },
            else => i.fail("Cannot subtract non-numeric type from integer", error.TypeError),
        },
        .float => switch (rhs) {
            .int => .{ .float = lhs.float - @as(f64, @floatFromInt(rhs.int)) },
            .float => .{ .float = lhs.float - rhs.float },
            else => i.fail("Cannot subtract non-numeric type from float", error.TypeError),
        },
        .list => |list| list: switch (rhs) {
            .int, .float => {
                for (0.., list.elems) |j, elem| list.elems[j] = try i.subtr(elem, rhs);
                break :list lhs;
            },
            .list => {
                const len = lhs.len().?;
                const elems = try i.arena.alloc(IValue, len);
                for (0..len) |j| elems[j] = try i.subtr(list.elems[j], rhs.list.elems[j]);
                const list_: List = .{ .elems = elems };
                break :list .{ .list = try list_.makePointer(i.arena) };
            },
            else => return i.fail("Values could not be broadcasted for the list", error.TypeError),
        },
        else => i.fail("Invalid type for subtraction operation", error.TypeError),
    };
}

fn mult(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    const _err = error.TypeError;
    if (lhs.isOneOf(&.{ .int, .float }) and rhs.isOneOf(&.{ .string, .list })) {
        return i.mult(rhs, lhs);
    }

    return switch (lhs) {
        .int => switch (rhs) {
            .int => .{ .int = lhs.int * rhs.int },
            .float => .{ .float = @as(f64, @floatFromInt(lhs.int)) * rhs.float },
            else => i.fail("Cannot multiply integer by non-numeric type", _err),
        },
        .float => switch (rhs) {
            .int => .{ .float = lhs.float * @as(f64, @floatFromInt(rhs.int)) },
            .float => .{ .float = lhs.float * rhs.float },
            else => i.fail("Cannot multiply float by non-numeric type", _err),
        },
        .string => |string| str: {
            if (rhs.is(.int)) {
                if (rhs.int < 1) {
                    break :str i.fail("String multiplier must not be negative", _err);
                }
                const smu: usize = @intCast(rhs.int);
                var buf = try i.arena.alloc(u8, string.len * smu);
                for (0..smu) |j| {
                    const start = j * string.len;
                    @memcpy(buf[start .. start + string.len], string);
                }
                break :str .{ .string = buf };
            } else break :str i.fail("Values could not be broadcasted for the string", _err);
        },
        .list => |list| list: switch (rhs) {
            .int, .float => {
                for (0.., list.elems) |j, elem| list.elems[j] = try i.mult(elem, rhs);
                break :list lhs;
            },
            .list => {
                const len = lhs.len().?;
                const elems = try i.arena.alloc(IValue, len);
                for (0..len) |j| elems[j] = try i.mult(list.elems[j], rhs.list.elems[j]);
                const list_: List = .{ .elems = elems };
                break :list .{ .list = try list_.makePointer(i.arena) };
            },
            else => break :list i.fail("Values could not be broadcasted for the list", _err),
        },
        else => i.fail("Invalid type for multiplication", error.TypeError),
    };
}
fn div(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    const _err = error.TypeError;
    return switch (lhs) {
        .int => switch (rhs) {
            .int => .{ .int = @divFloor(lhs.int, rhs.int) },
            .float => .{ .float = @as(f64, @floatFromInt(lhs.int)) / rhs.float },
            else => i.fail("Cannot divide integer by non-numeric type", _err),
        },
        .float => switch (rhs) {
            .int => .{ .float = lhs.float / @as(f64, @floatFromInt(rhs.int)) },
            .float => .{ .float = lhs.float / rhs.float },
            else => i.fail("Cannot divide float by non-numeric type", _err),
        },
        else => i.fail("Division is only supported for numbers", _err),
    };
}

fn power(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    const _err = error.TypeError;
    return switch (lhs) {
        .int => switch (rhs) {
            .int => .{ .int = std.math.pow(i32, lhs.int, rhs.int) },
            .float => .{ .float = std.math.pow(f64, @as(f64, @floatFromInt(lhs.int)), rhs.float) },
            else => i.fail("Cannot raise integer to non-numeric power", _err),
        },
        .float => switch (rhs) {
            .int => .{ .float = std.math.pow(f64, lhs.float, @as(f64, @floatFromInt(rhs.int))) },
            .float => .{ .float = std.math.pow(f64, lhs.float, rhs.float) },
            else => i.fail("Cannot raise float to non-numeric power", _err),
        },
        else => i.fail("Power is only supported for numbers", _err),
    };
}

fn equal(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    _ = i;
    return .{ .boolean = deepEqual(lhs, rhs) };
}

fn notEqual(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    return .{ .boolean = !(try i.equal(lhs, rhs)).boolean };
}

fn lessThan(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    const _reason = "Can only tell whether one number is less than (LT) another";
    const _err = error.TypeError;
    return switch (lhs) {
        .int => switch (rhs) {
            .int => .{ .boolean = lhs.int < rhs.int },
            .float => .{ .boolean = @as(f64, @floatFromInt(lhs.int)) < rhs.float },
            else => i.fail(_reason, _err),
        },
        .float => switch (rhs) {
            .int => .{ .boolean = lhs.float < @as(f64, @floatFromInt(rhs.int)) },
            .float => .{ .boolean = lhs.float < rhs.float },
            else => i.fail(_reason, _err),
        },
        else => i.fail(_reason, _err),
    };
}

fn lessOrEqualThan(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    const _reason = "Can only tell whether one number is less or equal than (LET) another";
    const _err = error.TypeError;
    return switch (lhs) {
        .int => switch (rhs) {
            .int => .{ .boolean = lhs.int <= rhs.int },
            .float => .{ .boolean = @as(f64, @floatFromInt(lhs.int)) <= rhs.float },
            else => i.fail(_reason, _err),
        },
        .float => switch (rhs) {
            .int => .{ .boolean = lhs.float <= @as(f64, @floatFromInt(rhs.int)) },
            .float => .{ .boolean = lhs.float <= rhs.float },
            else => i.fail(_reason, _err),
        },
        else => i.fail(_reason, _err),
    };
}

fn greaterThan(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    const _reason = "Can only tell whether one number is greater than (GT) another";
    const _err = error.TypeError;
    return switch (lhs) {
        .int => switch (rhs) {
            .int => .{ .boolean = lhs.int > rhs.int },
            .float => .{ .boolean = @as(f64, @floatFromInt(lhs.int)) > rhs.float },
            else => i.fail(_reason, _err),
        },
        .float => switch (rhs) {
            .int => .{ .boolean = lhs.float > @as(f64, @floatFromInt(rhs.int)) },
            .float => .{ .boolean = lhs.float > rhs.float },
            else => i.fail(_reason, _err),
        },
        else => i.fail(_reason, _err),
    };
}

fn greaterOrEqualThan(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    const _reason = "Can only tell whether one number is greater or equal than (GET) another";
    const _err = error.TypeError;
    return switch (lhs) {
        .int => switch (rhs) {
            .int => .{ .boolean = lhs.int >= rhs.int },
            .float => .{ .boolean = @as(f64, @floatFromInt(lhs.int)) >= rhs.float },
            else => i.fail(_reason, _err),
        },
        .float => switch (rhs) {
            .int => .{ .boolean = lhs.float >= @as(f64, @floatFromInt(rhs.int)) },
            .float => .{ .boolean = lhs.float >= rhs.float },
            else => i.fail(_reason, _err),
        },
        else => i.fail(_reason, _err),
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
            if (lhs != .string) return i.fail("Both sides must be strings", null);
            return .{ .boolean = std.mem.indexOf(u8, str, lhs.string) != null };
        },
        .list => |list| {
            for (list.elems) |elem| {
                if (deepEqual(lhs, elem)) return .{ .boolean = true };
            }
            return .{ .boolean = false };
        },
        .hash_map => |hash_map| return .{ .boolean = hash_map.inner.contains(lhs) },
        else => return i.fail("Cannot tell whether LHS is in RHS (invalid type)", null),
    }
}

fn condExpr(i: *Interpreter, cond_expr: ast.CondExpr) !IValue {
    const if_cond = try i.visit(cond_expr.if_cond);
    return if (isTruthy(if_cond)) i.visit(cond_expr.then) else i.visit(cond_expr.else_expr);
}

inline fn isTruthy(value: IValue) bool {
    return switch (value) {
        .int, .float => |num| num != 0,
        .boolean => |boolean| boolean,
        .string, .list, .hash_map => value.len().? > 0,
        else => unreachable,
    };
}

// ------------------------------------------------------------------------------------------------
// Collections, statements and definitions
// ------------------------------------------------------------------------------------------------

fn indexExpr(i: *Interpreter, index_expr: ast.IndexExpr) !IValue {
    const target = try i.visit(index_expr.target);
    const index = try i.visit(index_expr.index);
    // TODO: Support indexing into IMatrix.
    return switch (target) {
        .hash_map => |hash_map| hash_map.get(index) orelse .none,
        .list => |list| list: {
            if (index.int >= target.len().?)
                break :list i.fail("List index out of bounds", null);
            break :list list.get(@intCast(index.int));
        },
        else => i.fail("Could not index into the type", null),
    };
}

fn assignStmt(i: *Interpreter, assign_stmt: ast.AssignStmt) !IValue {
    const value: IValue = i.visit(assign_stmt.value) catch |err| switch (err) {
        error.TypeError => .{ .lazy_index = assign_stmt.value },
        else => return err,
    };
    try i.setVar(.global, assign_stmt.name, value);
    return value;
}

fn fnDef(i: *Interpreter, fn_index: ast.Index, fn_def: ast.FnDef) !IValue {
    try i.setVar(.global, fn_def.name, .{ .fn_index = fn_index });
    return .none;
}

fn returnStmt(i: *Interpreter, return_stmt: ast.ReturnStmt) !IValue {
    i.return_value = try i.visit(return_stmt.value);
    return Error.ReturnTrigger;
}

fn forStmt(i: *Interpreter, for_stmt: ast.ForStmt) !IValue {
    const iter = try i.visit(for_stmt.iter);
    const variable = for_stmt.variable;
    const start, const len = .{ for_stmt.body_start, for_stmt.body_len };

    switch (iter) {
        .list => |list| {
            for (list.elems) |elem| {
                try i.setVar(.local, variable, elem);
                _ = try i.block(start, len);
                try i.setVar(.local, variable, .none);
            }
        },
        .string => |string| {
            for (0..string.len) |j| {
                try i.setVar(.local, variable, .{ .string = string[j .. j + 1] });
                _ = try i.block(start, len);
                try i.setVar(.local, variable, .none);
            }
        },
        .hash_map => |hash_map| {
            var it = hash_map.inner.keyIterator();
            while (it.next()) |key_ptr| {
                try i.setVar(.local, variable, key_ptr.*);
                _ = try i.block(start, len);
                try i.setVar(.local, variable, .none);
            }
        },
        else => return i.fail("IValue is not iterable", null),
    }

    return .none;
}

fn block(i: *Interpreter, start: ast.Index, len: ast.Index) !IValue {
    for (0..len) |offset| {
        const stmt_node_index = i.tree.adpb[start + offset];

        _ = i.visit(stmt_node_index) catch |err| switch (err) {
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

// ------------------------------------------------------------------------------------------------
// Call syntax: traditional, lazy, short-circuit evaluation and chaining
// ------------------------------------------------------------------------------------------------

fn call(i: *Interpreter, call_: ast.Call) !IValue {
    const callable = switch (call_.callable) {
        .ident => |ident| try i.getVar(ident),
        .expr => return i.callExpr(call_),
    };

    var args = try i.arena.alloc(IValue, call_.args_len);
    for (0..call_.args_len) |offset| {
        const arg_node_index = i.tree.adpb[@intCast(call_.args_start + offset)];
        args[offset] = try i.visit(arg_node_index);
    }

    return switch (callable) {
        .func => |func| func(i, args),
        .fn_index => |fn_index| i.callFn(fn_index, args),
        .lazy_index => |lazy_index| i.callExpr(.{
            .callable = .{ .expr = lazy_index },
            .args_start = call_.args_start,
            .args_len = call_.args_len,
        }),
        else => i.fail("IValue is not callable", null),
    };
}

fn callExpr(i: *Interpreter, expr_call: ast.Call) Error!IValue {
    const node: ast.Node = i.getNode(expr_call.callable.expr);
    const args: CallArgs = .{
        .start = expr_call.args_start,
        .len = expr_call.args_len,
    };
    return switch (node) {
        .ident => |ident| i.callExprIdent(ident, args),
        .bin_expr => |bin_expr| i.callBinExpr(bin_expr, args),
        .cond_expr => |cond_expr| i.callCondExpr(cond_expr, args),
        else => i.fail("This type of node is disallowed/unsupported", null),
    };
}

fn callBinExpr(i: *Interpreter, bin_expr: ast.BinExpr, args: CallArgs) !IValue {
    const lhs = try i.cvisit(bin_expr.lhs, args);
    const lhs_allowed = &.{ .int, .string, .boolean };
    const rhs = try i.cvisit(bin_expr.rhs, args);
    if (lhs.isOneOf(lhs_allowed) and rhs.is(.list)) {
        return i.callBinExpr(.{
            .lhs = bin_expr.rhs,
            .op = bin_expr.op,
            .rhs = bin_expr.lhs,
        }, args);
    }

    if (lhs.is(.list) and bin_expr.op.isComp()) {
        const len = lhs.len().?;
        const elems = try i.arena.alloc(IValue, len);
        for (0..len) |j| {
            const res = try i.bin(bin_expr.op, lhs.list.elems[j], rhs);
            elems[j] = res;
        }
        const list: List = .{ .elems = elems };
        return .{ .list = try list.makePointer(i.arena) };
    } else return i.bin(bin_expr.op, lhs, rhs);
}

fn callCondExpr(i: *Interpreter, cond_expr: ast.CondExpr, args: CallArgs) !IValue {
    const if_cond = try i.cvisit(cond_expr.if_cond, args);
    if (isTruthy(if_cond)) {
        return i.cvisit(cond_expr.then, args);
    } else {
        return i.cvisit(cond_expr.else_expr, args);
    }
}

fn cvisit(i: *Interpreter, index: ast.Index, args: CallArgs) !IValue {
    const node = i.getNode(index);
    if (node.is(.ident)) {
        return i.callExprIdent(node.ident, args);
    } else if (node.is(.bin_expr)) {
        return i.callExpr(.{
            .callable = .{ .expr = index },
            .args_start = args.start,
            .args_len = args.len,
        });
    } else {
        return i.visit(index);
    }
}

fn callExprIdent(i: *Interpreter, ident: []const u8, args: CallArgs) Error!IValue {
    const value = try i.getVar(ident);
    return switch (value) {
        .func, .fn_index => i.call(.{
            .callable = .{ .ident = ident },
            .args_start = args.start,
            .args_len = args.len,
        }),
        else => value,
    };
}

// TODO: Call chaining (functions as arguments).
fn callChain(i: *Interpreter, outer: []const u8, inner: ast.Index, args: CallArgs) !IValue {
    _ = i;
    _ = outer;
    _ = inner;
    _ = args;
}

const CallArgs = struct { start: ast.Index, len: u32 };

fn callFn(i: *Interpreter, fn_index: ast.Index, args: []IValue) !IValue {
    const fn_def = i.tree.nodes[fn_index].fn_def;

    const init_local = i.local;
    i.local = Table.init(i.arena);
    defer {
        i.local.deinit();
        i.local = init_local;
    }

    if (args.len != fn_def.args_len) return i.fail("Argument count mismatch", null);

    for (0..fn_def.args_len) |offset| {
        const param_node_index = i.tree.adpb[fn_def.args_start + offset];
        const param_name = i.tree.nodes[param_node_index].ident;
        try i.setVar(.local, param_name, args[offset]);
    }
    return i.block(fn_def.body_start, fn_def.body_len);
}

fn opArg(i: *Interpreter, bin_op: ast.BinOp) !IValue {
    if (bin_op.isComp()) return .{ .op_arg = bin_op } else {
        return i.fail("Invalid operational argument", null);
    }
}

// ------------------------------------------------------------------------------------------------
// Built-in functions
// ------------------------------------------------------------------------------------------------

fn builtinPrint(i: *Interpreter, args: []IValue) !IValue {
    for (args, 0..) |arg, j| {
        switch (arg) {
            .float, .int, .string, .boolean, .list, .hash_map, .matrix => try arg.repr(i.writer),
            else => i.iprint("IValue: {any}", .{arg}),
        }
        if (j < args.len - 1) i.iprint(" ", .{});
    }
    i.iprint("\n", .{});
    return .none;
}

/// Infallible print.
fn iprint(i: *Interpreter, comptime format: []const u8, args: anytype) void {
    i.writer.print(format, args) catch unreachable;
}

fn builtinTokens(i: *Interpreter, args: []IValue) !IValue {
    if (args.len != 1) {
        return i.fail("Expected only 1 string as an argument", error.BuiltinFailed);
    }

    const string: []const u8 = args[0].string;
    const elems = try i.arena.alloc(IValue, args[0].len().?);
    for (0.., string) |j, char| elems[j] = .{ .string = try i.arena.dupe(u8, &[1]u8{char}) };
    const list: List = .{ .elems = elems };
    return .{ .list = try list.makePointer(i.arena) };
}

fn builtinIndicator(i: *Interpreter, args: []IValue) !IValue {
    if (args.len != 1) {
        return i.fail("Expected only 1 list of booleans as an argument", error.BuiltinFailed);
    }

    const value = args[0];
    const elems = try i.arena.alloc(IValue, value.len().?);
    for (0.., value.list.elems) |j, elem| {
        elems[j] = if (elem.boolean) .{ .int = 1 } else .{ .int = 0 };
    }
    const list: List = .{ .elems = elems };
    return .{ .list = try list.makePointer(i.arena) };
}

fn builtinIndices(i: *Interpreter, args: []IValue) !IValue {
    if (args.len != 1) {
        return i.fail("Expected only 1 string as an argument", error.BuiltinFailed);
    }

    const len = args[0].len() orelse {
        return i.fail("Invalid input type for `Indices(...)`", error.BuiltinFailed);
    };
    const elems = try i.arena.alloc(IValue, len);
    for (0..len) |j| elems[j] = .{ .int = @intCast(j) };
    const list: List = .{ .elems = elems };
    return .{ .list = try list.makePointer(i.arena) };
}

fn builtinSelect(i: *Interpreter, args: []IValue) !IValue {
    if (args.len != 3) {
        const _reason = "Expected exactly 3 arguments: 2 collections and 1 operational argument";
        return i.fail(_reason, error.BuiltinFailed);
    }

    const lhs, const rhs = .{ args[0].list, args[1].list };
    const op_arg = args[2].op_arg;
    var matrix: IMatrix = try .init(i.arena, @intCast(rhs.elems.len), @intCast(lhs.elems.len));

    for (0.., rhs.elems) |row, rhs_elem| {
        for (0.., lhs.elems) |column, lhs_elem| {
            const matrix_elem = if (op_arg.isComp()) try i.bin(op_arg, lhs_elem, rhs_elem) else {
                unreachable;
            };
            matrix.set(@intCast(row), @intCast(column), matrix_elem);
        }
    }
    return .{ .matrix = try matrix.makePointer(i.arena) };
}

fn builtinAggregate(i: *Interpreter, args: []IValue) Error!IValue {
    if (args.len != 2) {
        const _reason = "Exactly 2 arguments expected: selector matrix and a collection";
        return i.fail(_reason, error.BuiltinFailed);
    }

    const selector = args[0].matrix;
    const in = args[1].list;
    var elems = try i.arena.alloc(IValue, in.elems.len);
    for (0..selector.rows) |row| {
        var acc: f64, var agg: u32 = .{ 0, 0 };
        for (0.., in.elems) |column, in_elem| {
            if (selector.get(@intCast(row), @intCast(column)).boolean) {
                acc += in_elem.int;
                agg += 1;
            }
        }
        elems[row] = .{ .float = acc / agg };
    }
    const list: List = .{ .elems = elems };
    return .{ .list = try list.makePointer(i.arena) };
}

test {
    _ = @import("Interpreter.zig");
}

const std = @import("std");
const meta = std.meta;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
