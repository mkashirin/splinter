pub const Parser = struct {
    gpa: Allocator,

    tokenizer: *Tokenizer,
    current: Token = undefined,
    upcoming: Token = undefined,

    nodes: ArrayList(Node) = .empty,
    adpb: IndicesList = .empty,
    diagnostic: ?Diagnostic = null,
    const Self = @This();

    pub const Diagnostic = struct {
        expected: Expected,
        found: Token,

        pub const Expected = union(enum) {
            tag: Tag,
            description: []const u8,
        };
    };

    pub fn init(tokenizer: *Tokenizer, gpa: Allocator) !Self {
        var self: Self = .{ .tokenizer = tokenizer, .gpa = gpa };
        self.step();
        self.peek();
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.tokenizer.deinit();
        self.nodes.deinit(self.gpa);
        self.* = undefined;
    }

    pub fn buildTree(self: *Self) Error!Tree {
        var indices = IndicesList.empty;
        errdefer indices.deinit(self.gpa);
        while (!self.match(.eof)) {
            const stmt_index = try self.stmt();
            try indices.append(self.gpa, stmt_index);
        }
        return .init(self.gpa, &indices, &self.nodes, &self.adpb);
    }

    fn stmt(self: *Self) Error!Index {
        self.peek();
        return switch (self.current.tag) {
            .ident => if (self.matchUpcoming(.equal))
                self.assignStmt()
            else
                self.exprStmt(),
            .keyword_def => self.fnDef(),
            .keyword_return => self.returnStmt(),
            .keyword_for => self.forStmt(),
            else => self.exprStmt(),
        };
    }

    fn peek(self: *Self) void {
        self.upcoming = self.tokenizer.peek();
    }

    fn assignStmt(self: *Self) !Index {
        var variable = try self.condExpr();
        if (!self.match(.equal)) return variable;

        const node = self.nodes.items[@intCast(variable)];
        switch (node) {
            .ident => |name| {
                self.step();

                const value = try self.assignStmt();
                const assign_stmt: AssignStmt = .{
                    .name = name,
                    .value = value,
                };

                variable = try self.push(.{ .assign_stmt = assign_stmt });
                try self.expect(.semicolon);
                self.step();
                return variable;
            },
            else => return self.fail(.{ .description = "expression" }),
        }
    }

    fn fnDef(self: *Self) !Index {
        try self.expect(.keyword_def);
        self.step();

        try self.expect(.ident);
        const name = self.current.lexeme.?;
        self.step();

        try self.expect(.left_paren);
        self.step();

        const args_start: Index = @intCast(self.adpb.items.len);
        while (true) {
            try self.expect(.ident);
            const arg = try self.push(.{ .ident = self.current.lexeme.? });

            try self.adpb.append(self.gpa, arg);
            self.step();

            if (self.match(.right_paren)) break;
            try self.expect(.comma);
            self.step();
        }
        const args_len = @as(Index, @intCast(self.adpb.items.len)) - args_start;
        self.step();

        try self.expect(.left_brace);
        self.step();

        const body_start: Index = @intCast(self.adpb.items.len);
        while (!self.match(.right_brace)) {
            const stmt_index = try self.stmt();
            try self.adpb.append(self.gpa, stmt_index);
        }
        const body_len = @as(Index, @intCast(self.adpb.items.len)) - body_start;
        self.step();

        const fn_def: FnDef = .{
            .name = name,
            .args_start = args_start,
            .args_len = args_len,
            .body_start = body_start,
            .body_len = body_len,
        };
        return self.push(.{ .fn_def = fn_def });
    }

    fn returnStmt(self: *Self) !Index {
        self.step();
        const value = try self.expr();
        try self.expect(.semicolon);

        self.step();
        const return_stmt: ReturnStmt = .{ .value = value };
        return self.push(.{ .return_stmt = return_stmt });
    }

    fn forStmt(self: *Self) !Index {
        try self.expect(.keyword_for);
        self.step();

        try self.expect(.ident);
        const variable = self.current.lexeme.?;
        self.step();

        try self.expect(.keyword_in);
        self.step();

        const iterable = try self.expr();
        try self.expect(.left_brace);
        self.step();

        const body_start: Index = @intCast(self.adpb.items.len);
        while (!self.match(.right_brace)) {
            const stmt_index = try self.stmt();
            try self.adpb.append(self.gpa, stmt_index);
        }
        const body_len = @as(Index, @intCast(self.adpb.items.len)) - body_start;
        self.step();

        const for_stmt: ForStmt = .{
            .variable = variable,
            .iterable = iterable,
            .body_start = body_start,
            .body_len = body_len,
        };
        return self.push(.{ .for_stmt = for_stmt });
    }

    fn exprStmt(self: *Self) Error!Index {
        const res = self.expr();
        try self.expect(.semicolon);
        self.step();
        return res;
    }

    fn expr(self: *Self) Error!Index {
        return self.condExpr();
    }

    fn condExpr(self: *Self) !Index {
        const then = try self.inAndOrExpr();
        if (!self.match(.keyword_if)) return then;
        self.step();

        const if_cond = try self.inAndOrExpr();
        try self.expect(.keyword_else);
        self.step();

        const else_expr = try self.inAndOrExpr();
        const cond_expr: CondExpr = .{
            .then = then,
            .if_cond = if_cond,
            .else_expr = else_expr,
        };
        return self.push(.{ .cond_expr = cond_expr });
    }

    fn inAndOrExpr(self: *Self) !Index {
        var lhs = try self.compExpr();
        while (true) {
            const op: BinOp = switch (self.current.tag) {
                .keyword_in => .is_in,
                .keyword_and => .logic_and,
                .keyword_or => .logic_or,
                else => break,
            };
            self.step();

            const rhs = try self.compExpr();
            const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.push(.{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn compExpr(self: *Self) !Index {
        var lhs = try self.addSubtrExpr();
        while (true) {
            const op: BinOp = switch (self.current.tag) {
                .double_equal => .equal,
                .bang_equal => .not_equal,
                .greater_than => .greater_than,
                .greater_or_equal_than => .greater_or_equal_than,
                .less_than => .less_than,
                .less_or_equal_than => .less_or_equal_than,

                else => break,
            };
            self.step();

            const rhs = try self.addSubtrExpr();
            const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.push(.{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn addSubtrExpr(self: *Self) !Index {
        var lhs = try self.multDivPowExpr();
        while (true) {
            const op: BinOp = switch (self.current.tag) {
                .plus => .add,
                .minus => .subtr,
                else => break,
            };
            self.step();

            const rhs = try self.multDivPowExpr();
            const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.push(.{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn multDivPowExpr(self: *Self) !Index {
        var lhs = try self.primaryExpr();
        while (true) {
            const op: BinOp = switch (self.current.tag) {
                .star => .mult,
                .carrot => .power,
                .slash => .div,
                else => break,
            };
            self.step();

            const rhs = try self.multDivPowExpr();
            const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.push(.{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn primaryExpr(self: *Self) !Index {
        var primary: Index = try switch (self.current.tag) {
            .ident => self.nameExpr(),
            .int_literal => self.intLiteral(),
            .string_literal => self.stringLiteral(),
            .left_brace => self.hashMapLiteral(),
            .left_bracket => self.listLiteral(),
            .left_paren => self.boxedExpr(),

            .keyword_true, .keyword_false => self.booleanLiteral(),
            else => self.fail(.{ .description = "expression" }),
        };
        primary = try self.indexExpr(primary);
        return primary;
    }

    fn indexExpr(self: *Self, target: Index) !Index {
        while (self.match(.left_bracket)) {
            self.step();

            const index = try self.expr();
            try self.expect(.right_bracket);
            self.step();

            const index_expr: IndexExpr = .{ .target = target, .index = index };
            return self.push(.{ .index_expr = index_expr });
        }
        return target;
    }

    /// This function is rather specific. Not only this one handles identifiers
    /// and function calls, it also targets the only language built-in, that is
    /// capable of accepting bare comparison predicates as an argument
    /// (`Select` function).
    fn nameExpr(self: *Self) !Index {
        const name = self.current.lexeme.?;
        self.step();

        if (!self.match(.left_paren))
            return self.push(.{ .ident = name });
        self.step();

        const args_start: Index = @intCast(self.adpb.items.len);
        while (true) {
            const arg = self.expr() catch blk: {
                const arg_count =
                    @as(Index, @intCast(self.adpb.items.len)) - args_start;

                if (!std.mem.eql(u8, name, "Select") or arg_count != 2)
                    return self.fail(.{ .description = "bin comp" });

                const op_arg: BinOp = switch (self.current.tag) {
                    .double_equal => .equal,
                    .bang_equal => .not_equal,
                    .greater_than => .greater_than,
                    .greater_or_equal_than => .greater_or_equal_than,
                    .less_than => .less_than,
                    .less_or_equal_than => .less_or_equal_than,

                    else => return self.fail(.{ .description = "bin comp" }),
                };
                self.step();

                break :blk try self.push(.{ .op_arg = op_arg });
            };
            try self.adpb.append(self.gpa, arg);

            if (self.match(.right_paren)) break;
            try self.expect(.comma);
            self.step();
        }
        self.step();

        const args_len =
            @as(Index, @intCast(self.adpb.items.len)) - args_start;
        const call: FnCall = .{
            .name = name,
            .args_start = args_start,
            .args_len = args_len,
        };
        return self.push(.{ .fn_call = call });
    }

    fn intLiteral(self: *Self) !Index {
        const int = try fmt.parseInt(i64, self.current.lexeme.?, 10);
        const index = try self.push(.{ .int = int });
        self.step();
        return index;
    }

    fn stringLiteral(self: *Self) !Index {
        const string = self.current.lexeme.?;
        const index = try self.push(.{ .string = string });
        self.step();
        return index;
    }

    fn booleanLiteral(self: *Self) !Index {
        const index = switch (self.current.tag) {
            .keyword_true => self.push(.{ .boolean = true }),
            .keyword_false => self.push(.{ .boolean = false }),
            else => unreachable,
        };
        self.step();
        return index;
    }

    fn listLiteral(self: *Self) !Index {
        self.step();
        const expr_ = try self.expr();
        if (!self.match(.keyword_for)) {
            var elems = IndicesList.empty;
            errdefer elems.deinit(self.gpa);
            try elems.append(self.gpa, expr_);
            while (self.match(.comma)) {
                self.step();
                if (self.match(.right_bracket)) break;
                const elem = try self.expr();
                try elems.append(self.gpa, elem);
            }
            self.step();

            const list: List = try .init(self.gpa, &elems);
            return self.push(.{ .list = list });
        }

        self.step();
        try self.expect(.ident);
        const variable = self.current.lexeme.?;
        self.step();

        try self.expect(.keyword_in);
        self.step();

        const iterable = try self.expr();
        try self.expect(.right_bracket);
        self.step();
        const list_comp: ListComp = .{
            .expr = expr_,
            .variable = variable,
            .iterable = iterable,
        };
        return self.push(.{ .list_comp = list_comp });
    }

    fn hashMapLiteral(self: *Self) !Index {
        self.step();
        var keys, var values = .{ IndicesList.empty, IndicesList.empty };
        errdefer {
            keys.deinit(self.gpa);
            values.deinit(self.gpa);
        }
        while (true) {
            const key = try self.expr();
            try keys.append(self.gpa, key);
            try self.expect(.colon);
            self.step();

            const value = try self.expr();
            try values.append(self.gpa, value);
            if (self.match(.right_brace)) break;
            try self.expect(.comma);
            self.step();
        }
        self.step();
        const hash_map: HashMap = try .init(self.gpa, &keys, &values);
        return self.push(.{ .hash_map = hash_map });
    }

    fn push(self: *Self, node: Node) Allocator.Error!Index {
        try self.nodes.append(self.gpa, node);
        const index: Index = @intCast(self.nodes.items.len - 1);
        return index;
    }

    fn boxedExpr(self: *Self) !Index {
        self.step();
        const index = try self.expr();
        try self.expect(.right_paren);

        self.step();
        return index;
    }

    fn step(self: *Self) void {
        self.current = self.tokenizer.next();
    }

    fn match(self: *Self, with: Tag) bool {
        return self.current.tag == with;
    }

    fn matchUpcoming(self: *Self, with: Tag) bool {
        return self.upcoming.tag == with;
    }

    fn expect(self: *Self, expected: Tag) Error!void {
        if (self.current.tag != expected)
            return self.fail(.{ .tag = expected });
    }

    fn fail(self: *Self, reason: Diagnostic.Expected) Error {
        self.diagnostic = .{ .expected = reason, .found = self.current };
        return Error.SyntaxParseError;
    }
};

pub const Error = Allocator.Error || fmt.ParseIntError ||
    error{SyntaxParseError};

pub const Tree = struct {
    indices: []const Index,
    nodes: []const Node,
    adpb: []const Index,
    const Self = @This();

    pub fn init(
        gpa: Allocator,
        indices: *IndicesList,
        nodes: *ArrayList(Node),
        adpb: *IndicesList,
    ) !Self {
        return .{
            .indices = try indices.toOwnedSlice(gpa),
            .nodes = try nodes.toOwnedSlice(gpa),
            .adpb = try adpb.toOwnedSlice(gpa),
        };
    }

    pub fn deinit(self: *Self, gpa: Allocator) void {
        for (self.nodes) |node| switch (node) {
            .list => |*list| list.deinit(gpa),
            .hash_map => |*hash_map| hash_map.deinit(gpa),
            else => {},
        };
        gpa.free(self.indices);
        gpa.free(self.nodes);
        gpa.free(self.adpb);
        self.* = undefined;
    }
};

pub const Node = union(enum) {
    int: i64,
    string: []const u8,
    boolean: bool,
    ident: []const u8,
    list: List,
    list_comp: ListComp,
    hash_map: HashMap,

    bin_expr: BinExpr,
    cond_expr: CondExpr,
    index_expr: IndexExpr,

    assign_stmt: AssignStmt,
    fn_def: FnDef,
    return_stmt: ReturnStmt,
    fn_call: FnCall,
    op_arg: BinOp,
    for_stmt: ForStmt,
};

pub const BinExpr = struct { lhs: Index, op: BinOp, rhs: Index };

pub const BinOp = enum {
    add,
    subtr,
    mult,
    power,
    div,

    equal,
    not_equal,
    greater_than,
    greater_or_equal_than,
    less_than,
    less_or_equal_than,

    logic_and,
    logic_or,
    is_in,
};

pub const FnCall = struct {
    name: []const u8,
    args_start: Index,
    args_len: Index,
};

pub const CondExpr = struct {
    then: Index,
    if_cond: Index,
    else_expr: Index,
};
pub const AssignStmt = struct { name: []const u8, value: Index };

pub const FnDef = struct {
    name: []const u8,
    args_start: Index,
    args_len: Index,
    body_start: Index,
    body_len: Index,
};

pub const ReturnStmt = struct { value: Index };

pub const ForStmt = struct {
    variable: []const u8,
    iterable: Index,
    body_start: Index,
    body_len: Index,
};

pub const List = struct {
    elems: []const Index,
    const Self = @This();

    pub fn init(gpa: Allocator, elems: *IndicesList) !Self {
        return .{ .elems = try elems.toOwnedSlice(gpa) };
    }

    pub fn deinit(self: *const Self, gpa: Allocator) void {
        gpa.free(self.elems);
    }
};

pub const ListComp = struct {
    expr: Index,
    variable: []const u8,
    iterable: Index,
};

pub const HashMap = struct {
    keys: []const Index,
    values: []const Index,
    const Self = @This();

    pub fn init(gpa: Allocator, keys: *IndicesList, values: *IndicesList) !Self {
        return .{
            .keys = try keys.toOwnedSlice(gpa),
            .values = try values.toOwnedSlice(gpa),
        };
    }

    pub fn deinit(self: *const Self, gpa: Allocator) void {
        gpa.free(self.keys);
        gpa.free(self.values);
    }
};

pub const IndexExpr = struct { target: Index, index: Index };

pub const IndicesList = ArrayList(Index);
pub const Index = u32;

test {
    const source =
        \\an_int = 4 / 2;
        \\the_int = 2^3;
        \\
        \\
        \\def add(a, b) {
        \\    sum = a + b;
        \\    return sum;
        \\}
        \\
        \\int_sum = add(a_int, the_int);
        \\print("Success") if c > 0 else print(0);
        \\
        \\0 if true and the_int - an_int else int_sum or "Huh?";
        \\
        \\a_list = [1, 2, 3];
        \\a_dict = {"integer": 1, "list": [2, 3]};
        \\the_list = [0, {"one": 1}, 2 + 3];
        \\
        \\zero = the_list[a_list[0]];
        \\
        \\for n in a_list {
        \\    print(n + 1);
        \\}
        \\
        \\zero_in_the_list = 0 in the_list;
        \\
        \\selector = Select([1, 2, 3], [3, 2, 1], !=);
        \\
        \\list_comp = [i + 1 if i > 0 else i for i in a_list];
    ;
    const ta = std.testing.allocator;

    var tokenizer: Tokenizer = .init(source);
    var parser: Parser = try .init(&tokenizer, ta);
    var tree: Tree = undefined;
    tree = parser.buildTree() catch |err| {
        const err_location = parser.current.location;
        const diagnostic = parser.diagnostic.?;
        std.debug.print(
            "Error at line {d}, column {d}: {any}\n",
            .{ err_location.line, err_location.column, diagnostic },
        );
        parser.deinit();
        return err;
    };
    tree.deinit(ta);
}

const std = @import("std");
const fmt = std.fmt;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const EnumArray = std.enums.EnumArray;

const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Tag = Token.Tag;
