tree: Tree,
gpa: Allocator,
global: Table,
local: Table,
const Interpreter = @This();

pub fn walkTree(i: *Interpreter) !void {
    _ = i;
}

pub fn init(tree: Tree, gpa: Allocator) Interpreter {
    return .{
        .tree = tree,
        .gpa = gpa,
        .global = .init(gpa),
        .local = .init(gpa),
    };
}

pub const IValue = union(enum) {
    int: i64,
    string: []const u8,
    boolean: bool,
    list: List,
    map: Map,
    builtin: *const fn (*Interpreter, []*IValue) anyerror!IValue,
    function: u32,
};

const IValuesList = std.ArrayList(*IValue);

const List = struct {
    elems: []const *IValue,
    const Self = @This();

    pub fn init(gpa: Allocator, elems: *IValuesList) !Self {
        return .{ .elems = try elems.toOwnedSlice(gpa) };
    }

    pub fn deinit(self: *Self, gpa: Allocator) void {
        for (self.elems) |elem| gpa.destroy(elem);
        gpa.free(self.elems); // Can this be omitted?
        self.* = undefined;
    }

    pub fn get(self: *Self, index: u32) IValue {
        return self.elems[index].*;
    }
};

const Map = struct {
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
            std.hash.autoHash(hasher, std.meta.activeTag(ivalue));

            switch (ivalue) {
                .int => |int| std.hash.autoHash(hasher, int),
                .boolean => |boolean| std.hash.autoHash(hasher, boolean),
                .string => |string| hasher.update(string),
                .list => |list| {
                    std.hash.autoHash(hasher, list.elems.len);
                    for (list.elems) |elem| {
                        deepHash(hasher, elem.*);
                    }
                },
                .map => |map| std.hash.autoHash(hasher, map.inner.count()),

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

    pub fn deinit(self: *Self, gpa: Allocator) void {
        for (self.keys) |key| gpa.destroy(key);
        gpa.free(self.keys);

        for (self.values) |value| gpa.destroy(value);
        gpa.free(self.values);
        self.* = undefined;
    }

    pub fn get(self: *Self, key: *IValue) ?*IValue {
        return self.inner.get(key);
    }
};

pub fn deepEqual(lhs: IValue, rhs: IValue) bool {
    if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;
    return switch (lhs) {
        .int => |int| int == rhs.int,
        .boolean => |boolean| boolean == rhs.boolean,
        .string => |string| std.mem.eql(u8, string, rhs.string),
        .list => |list| {
            if (list.elems.len != rhs.list.elems.len) return false;
            for (list.elems, rhs.list.elems) |elem, rhs_elem| {
                if (!deepEqual(elem.*, rhs_elem.*)) return false;
            }
            return true;
        },
        .map => |map| {
            if (map.inner.count() != rhs.map.inner.count()) return false;
            var it = map.inner.iterator();
            var rhs_it = rhs.map.inner.iterator();
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

pub fn visitNode(i: *Interpreter, index: NodeIndex) anyerror!IValue {
    const node: Node = i.tree.nodes[@intCast(index)];
    const res: IValue = switch (node) {
        .bin_expr => |bin_expr| try i.binExpr(bin_expr),
        .index_expr => |index_expr| try i.indexExpr(index_expr),
        .int => |int| .{ .int = int },
        .string => |string| .{ .string = string },
        .boolean => |boolean| .{ .boolean = boolean },
        .list => |list| .{ .list = try i.listLiteral(list) },
        .map => |map| .{ .map = try i.mapLiteral(map) },
        else => return error.UnsupportedNodeType,
    };
    return res;
}

fn mapLiteral(i: *Interpreter, map: ast.Map) !Map {
    const len = map.keys.len;
    var keys = try IValuesList.initCapacity(i.gpa, len);
    var values = try IValuesList.initCapacity(i.gpa, len);
    errdefer {
        for (keys.items) |item| i.gpa.destroy(item);
        keys.deinit(i.gpa);

        for (values.items) |item| i.gpa.destroy(item);
        values.deinit(i.gpa);
    }
    for (map.keys, map.values) |key, value| {
        const key_visited = try i.gpa.create(IValue);
        key_visited.* = try i.visitNode(key);
        keys.appendAssumeCapacity(@constCast(key_visited));

        const value_visited = try i.gpa.create(IValue);
        value_visited.* = try i.visitNode(value);
        values.appendAssumeCapacity(@constCast(value_visited));
    }
    return .init(i.gpa, &keys, &values);
}

fn listLiteral(i: *Interpreter, list: ast.List) !List {
    const len = list.elems.len;
    var elems = try std.ArrayList(*IValue).initCapacity(i.gpa, len);
    errdefer {
        for (elems.items) |item| i.gpa.destroy(item);
        elems.deinit(i.gpa);
    }
    for (list.elems) |elem| {
        const visited = try i.gpa.create(IValue);
        visited.* = try i.visitNode(elem);
        elems.appendAssumeCapacity(@constCast(visited));
    }
    return .init(i.gpa, &elems);
}

fn indexExpr(i: *Interpreter, node: ast.IndexExpr) !IValue {
    const target: Node = i.tree.nodes[@intCast(node.target)];
    switch (target) {
        .map => |map| {
            const key = try i.visitNode(node.index);
            for (0..map.keys.len) |j| {
                const index = try i.visitNode(map.keys[j]);
                const keys_match =
                    try equal(key, index);
                if (keys_match.boolean) return try i.visitNode(map.values[j]);
            }
            return error.NoSuchKey;
        },
        .list => |list| {
            const index = try i.visitNode(node.index);
            if (index.int >= list.elems.len) return error.IndexOutOfBounds;
            return try i.visitNode(list.elems[@intCast(index.int)]);
        },
        else => return error.UnsupportedType,
    }
}

fn binExpr(i: *Interpreter, node: ast.BinExpr) anyerror!IValue {
    const lhs, const rhs =
        .{ try i.visitNode(node.lhs), try i.visitNode(node.rhs) };

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

    const res: IValue = switch (meta.activeTag(lhs)) {
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

    const res: IValue = switch (meta.activeTag(lhs)) {
        .int => .{ .int = lhs.int - rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn mult(lhs: IValue, rhs: IValue) !IValue {
    const res: IValue = switch (meta.activeTag(lhs)) {
        .int => .{ .int = lhs.int * rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn div(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    const res: IValue = switch (meta.activeTag(lhs)) {
        .int => .{ .int = @divFloor(lhs.int, rhs.int) },
        else => return error.UnsupportedType,
    };
    return res;
}

fn power(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    const res: IValue = switch (meta.activeTag(lhs)) {
        .int => .{ .int = lhs.int ^ rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn equal(lhs: IValue, rhs: IValue) !IValue {
    return .{ .boolean = deepEqual(lhs, rhs) };
}

fn notEqual(lhs: IValue, rhs: IValue) !IValue {
    return .{ .boolean = !(try equal(lhs, rhs)).boolean };
}

fn lessThan(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    return switch (meta.activeTag(lhs)) {
        .int => .{ .boolean = lhs.int < rhs.int },
        else => error.UnsupportedType,
    };
}

fn lessOrEqualThan(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    return switch (meta.activeTag(lhs)) {
        .int => .{ .boolean = lhs.int <= rhs.int },
        else => error.UnsupportedType,
    };
}

fn greaterThan(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    return switch (meta.activeTag(lhs)) {
        .int => .{ .boolean = lhs.int > rhs.int },
        else => error.UnsupportedType,
    };
}

fn greaterOrEqualThan(lhs: IValue, rhs: IValue) !IValue {
    try checkTypeCompat(lhs, rhs);

    return switch (meta.activeTag(lhs)) {
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

fn isTruthy(ivalue: IValue) bool {
    return switch (ivalue) {
        .int => |int| int != 0,
        .boolean => |boolean| boolean,
        .string => |string| string.len > 0,
        .list => |list| list.elems.len > 0,
        .map => |map| map.inner.count() > 0,

        else => unreachable,
    };
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
            for (list.elems) |elem| {
                if (deepEqual(lhs, elem.*)) return .{ .boolean = true };
            }
            return .{ .boolean = false };
        },
        .map => |map| {
            var key = lhs;
            return .{ .boolean = map.inner.contains(&key) };
        },
        else => return error.UnsupportedType,
    };

    return result;
}

fn checkTypeCompat(lhs: IValue, rhs: IValue) !void {
    @branchHint(.cold);
    if (meta.activeTag(lhs) != meta.activeTag(rhs))
        return error.TypeMismatch;
}

const Table = std.StringHashMap(*IValue);

fn getVar(i: *Interpreter, name: []const u8) !*IValue {
    if (i.local.get(name)) |ivalue|
        return ivalue
    else if (i.global.get(name)) |ivalue| return ivalue;
    return error.NoDefinitionFound;
}

pub fn setVariable(i: *Interpreter, name: []const u8, ivalue: IValue) !void {
    const entry = try i.local.getOrPut(name);
    if (entry.found_existing) {
        const value = entry.value_ptr.*;
        switch (value.*) {
            .list => |list| list.deinit(i.gpa),
            .map => |map| map.deinit(i.gpa),
            else => {},
        }
    } else entry.value_ptr.* = try i.gpa.create(IValue);
    entry.value_ptr.*.* = ivalue;
}

const std = @import("std");
const meta = std.meta;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Tree = ast.Tree;
const Node = ast.Node;
const NodeIndex = ast.Index;
