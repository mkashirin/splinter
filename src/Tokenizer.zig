source: []const u8,
index: usize,
line: usize,
column: usize,
const Tokenizer = @This();

pub fn init(source: []const u8) Tokenizer {
    return .{ .source = source, .index = 0, .line = 1, .column = 1 };
}

pub fn deinit(t: *Tokenizer) void {
    t.* = undefined;
}

pub fn peek(t: *Tokenizer) Token {
    const init_pos = .{ t.index, t.line, t.column };
    const token = t.next();
    t.index, t.line, t.column = init_pos;
    return token;
}

pub fn next(t: *Tokenizer) Token {
    var token: Token = .{ .tag = .invalid };
    t.skipWhitespaces();
    if (t.index >= t.source.len)
        return .{ .tag = .eof, .lexeme = "EOF" };
    const start = t.index;
    const current = t.source[t.index];
    t.step();

    token.tag = sw: switch (current) {
        'a'...'z', 'A'...'Z', '_' => {
            while (t.index < t.source.len) {
                const sub = t.source[t.index];
                switch (sub) {
                    'a'...'z', 'A'...'Z', '0'...'9', '_' => t.step(),
                    else => break,
                }
            }
            const lexeme = t.source[start..t.index];
            if (Token.keyword(lexeme)) |tag|
                break :sw tag
            else {
                token.lexeme = lexeme;
                break :sw .ident;
            }
        },

        '0'...'9' => {
            while (t.index < t.source.len) {
                const digit = t.source[t.index];
                switch (digit) {
                    '0'...'9' => t.step(),
                    else => break,
                }
            }
            token.lexeme = t.source[start..t.index];
            break :sw .int_literal;
        },

        '"' => {
            t.step();
            while (t.index < t.source.len and
                t.source[t.index] != '"') t.step();
            t.step();
            token.lexeme = t.source[start + 1 .. t.index - 1];
            break :sw .string_literal;
        },

        '=' => if (t.match('=')) {
            t.step();
            break :sw .double_equal;
        } else .equal,
        '!' => if (t.match('=')) {
            t.step();
            break :sw .bang_equal;
        } else .invalid,
        '>' => if (t.match('=')) {
            t.step();
            break :sw .greater_or_equal_than;
        } else .greater_than,
        '<' => if (t.match('=')) {
            t.step();
            break :sw .less_or_equal_than;
        } else .less_than,

        else => |char| std.enums.fromInt(Token.Tag, char) orelse .invalid,
    };

    token.location = .{ .line = t.line, .column = t.column };
    return token;
}

pub const Token = struct {
    tag: Tag,
    lexeme: ?[]const u8 = null,
    location: Location = undefined,

    pub const Tag = enum(u8) {
        // zig fmt: off
        plus    = '+',
        minus   = '-',
        star    = '*',
        slash   = '/',
        carrot  = '^',

        left_paren      = '(',
        right_paren     = ')',
        left_bracket    = '[',
        right_bracket   = ']',
        left_brace      = '{',
        right_brace     = '}',
        comma           = ',',
        semicolon       = ';',
        colon           = ':',

        equal           = '=',
        less_than       = '<',
        greater_than    = '>',
        // zig fmt: on

        double_equal,
        bang_equal,
        less_or_equal_than,
        greater_or_equal_than,

        eof,
        invalid,
        ident,
        string_literal,
        int_literal,

        keyword_true,
        keyword_false,
        keyword_if,
        keyword_else,
        keyword_in,
        keyword_and,
        keyword_or,
        keyword_def,
        keyword_return,
        keyword_for,
    };

    pub const Location = struct { line: usize, column: usize };

    pub const keywords: std.StaticStringMap(Tag) = .initComptime(.{
        .{ "true", .keyword_true },
        .{ "false", .keyword_false },
        .{ "if", .keyword_if },
        .{ "else", .keyword_else },
        .{ "in", .keyword_in },
        .{ "and", .keyword_and },
        .{ "or", .keyword_or },
        .{ "def", .keyword_def },
        .{ "return", .keyword_return },
        .{ "for", .keyword_for },
    });

    pub fn keyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }
};

fn skipWhitespaces(t: *Tokenizer) void {
    while (t.index < t.source.len) {
        switch (t.source[t.index]) {
            ' ', '\t', '\n', '\r' => _ = t.step(),
            else => break,
        }
    }
}

fn step(t: *Tokenizer) void {
    if (t.match('\n'))
        t.column, t.line = .{ 1, t.line + 1 }
    else if (t.match('\t')) t.column += 8 else t.column += 1;
    t.index += 1;
}

fn match(t: *Tokenizer, with: u8) bool {
    return t.source[t.index] == with;
}

test {
    _ = @import("Tokenizer.zig");
}

const std = @import("std");
