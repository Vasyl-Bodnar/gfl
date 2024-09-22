const std = @import("std");
const parser = @import("parser.zig");
const Ast = parser.Ast;

// pub const Ast = union(enum) {
// int: u64,
// str: []const u8,
// mstr: []u8,
// chr: u8,
// id: []const u8,
// sym: []const u8,
// block: std.ArrayList(Ast),
// ugraph: *UnweightedGraph, wgraph: *WeightedGraph,
// eugraph: *ExtendableUnweightedGraph, ewgraph: *ExtendableWeightedGraph,
// lamb: *Lambda,
// def: *Def,
// call: *Call,
// none
// };

const InterpreterError = error{
    InvalidArgument,
    UnknownOperator,
    UnknownIdentifier,
    UnknownFunction,
    UnknownError,
    NotAFunction,
    VertexNotFound,
    OneArgumentExpected,
    ThreeArgumentsExpected,
    LeastTwoArgumentsExpected,
    LeastThreeArgumentsExpected,
    LeastFourArgumentsExpected,
    UnexpectedArguments,
    UnexpectedExpression,
};

fn isExpr(ast: Ast) bool {
    switch (ast) {
        .int, .str, .mstr, .chr, .id, .sym, .block, .ugraph, .wgraph, .eugraph, .ewgraph, .lamb, .call => return true,
        else => return false,
    }
}

fn isStmt(ast: Ast) bool {
    switch (ast) {
        .def => return true,
        else => return false,
    }
}

fn isGraph(ast: Ast) bool {
    switch (ast) {
        .ugraph, .wgraph, .eugraph, .ewgraph => return true,
        else => return false,
    }
}

fn isExtendableGraph(ast: Ast) bool {
    switch (ast) {
        .eugraph, .ewgraph => return true,
        else => return false,
    }
}

fn eqAst(a: Ast, b: Ast) bool {
    switch (a) {
        .int => return (b == .int and a.int == b.int),
        .chr => return (b == .chr and a.chr == b.chr),
        .str => return (b == .str and std.mem.eql(u8, a.str, b.str)),
        .mstr => return (b == .mstr and std.mem.eql(u8, a.str, b.str)),
        .id => return (b == .id and std.mem.eql(u8, a.str, b.str)),
        .sym => return (b == .sym and std.mem.eql(u8, a.str, b.str)),
        .lamb => return (b == .lamb and a.lamb.args.items.len == b.lamb.args.items.len and eqAst(a.lamb.code, b.lamb.code) and for (a.lamb.args.items, 0..) |arg, i| {
            if (!std.mem.eql(u8, arg, b.lamb.args.items[i])) {
                break false;
            }
        } else true),
        .call => return (b == .call and a.call.args.items.len == b.call.args.items.len and std.mem.eql(u8, a.call.name, b.call.name) and for (a.call.args.items, 0..) |arg, i| {
            if (!eqAst(arg, b.call.args.items[i])) {
                break false;
            }
        } else true),
        // Currently unimplemented
        .ugraph => return (b == .ugraph),
        .wgraph => return (b == .wgraph),
        .eugraph => return (b == .eugraph),
        .ewgraph => return (b == .ewgraph),
        else => return false,
    }
    return false;
}

pub const Interpreter = struct {
    allocator: std.mem.Allocator,
    definitions: std.StringHashMap(Ast),

    pub fn init(allocator: std.mem.Allocator) !Interpreter {
        var hash_map = std.StringHashMap(Ast).init(allocator);
        //var definitions = std.ArrayList(std.StringHashMap(Ast)).init(allocator);
        //try definitions.append(hash_map);
        return Interpreter{ .allocator = allocator, .definitions = hash_map };
    }

    fn prettyMat(self: Interpreter, mat: [][]bool) ![]const u8 {
        var pretty_mat = std.ArrayList(u8).init(self.allocator);
        for (mat, 0..) |row, i| {
            for (row) |cell| {
                if (cell) {
                    try pretty_mat.appendSlice("1 ");
                } else {
                    try pretty_mat.appendSlice("0 ");
                }
            }
            if (i < mat.len - 1) {
                try pretty_mat.append('\n');
            }
        }
        return pretty_mat.items;
    }

    fn prettyMatW(self: Interpreter, mat: [][]Ast) ![]const u8 {
        var pretty_mat = std.ArrayList(u8).init(self.allocator);
        for (mat, 0..) |row, i| {
            for (row) |cell| {
                try pretty_mat.appendSlice(try self.prettyStr(cell));
                try pretty_mat.appendSlice(" ");
            }
            if (i < mat.len - 1) {
                try pretty_mat.append('\n');
            }
        }
        return pretty_mat.items;
    }

    fn prettyStr(self: Interpreter, ast: Ast) anyerror![]const u8 {
        switch (ast) {
            .int => return std.fmt.allocPrint(self.allocator, "{d}", .{ast.int}),
            .str => return std.fmt.allocPrint(self.allocator, "\"{s}\"", .{ast.str}),
            .mstr => return std.fmt.allocPrint(self.allocator, "\"*{s}*\"", .{ast.mstr}),
            .chr => return std.fmt.allocPrint(self.allocator, "'{c}'", .{ast.chr}),
            .id => return std.fmt.allocPrint(self.allocator, "{s}", .{ast.id}),
            .sym => return std.fmt.allocPrint(self.allocator, "`{s}", .{ast.sym}),
            .ugraph => return prettyMat(self, ast.ugraph.mat),
            .wgraph => return prettyMatW(self, ast.wgraph.mat),
            //.eugraph => return prettyMat(self, ast.eugraph.mat.items),
            //.ewgraph => return prettyMatW(self, ast.ewgraph.mat.items),
            .lamb => return std.fmt.allocPrint(self.allocator, "\\{s} -> {s}", .{ ast.lamb.args.items, try self.prettyStr(ast.lamb.code) }),
            .call => {
                var pretty_args = std.ArrayList([]const u8).init(self.allocator);
                for (ast.call.args.items) |arg| {
                    try pretty_args.append(try self.prettyStr(arg));
                }
                return std.fmt.allocPrint(self.allocator, "{s}{s}", .{ ast.call.name, pretty_args.items });
            },
            .block => return std.fmt.allocPrint(self.allocator, "{any}", .{ast.block.items}),
            .none => return "none",
            else => return "-",
        }
    }

    fn binaryOpInt(self: *Interpreter, op: u8, args: []Ast) !Ast {
        var new_args = try self.allocator.alloc(Ast, args.len);
        for (args, 0..) |arg, i| {
            new_args[i] = try self.eval(arg);
            if (new_args[i] != .int) {
                std.debug.print("error: expected integer, got {any}\n", .{new_args[i]});
                return InterpreterError.InvalidArgument;
            }
        }
        var total = new_args[0].int;
        if (new_args.len == 1) {
            if (op == '-') {
                return .{ .int = -total };
            } else {
                return .{ .int = total };
            }
        }
        for (new_args[1..]) |arg| {
            switch (op) {
                '+' => total += arg.int,
                '-' => total -= arg.int,
                '*' => total *= arg.int,
                '/' => total = @divFloor(total, arg.int),
                '%' => total = @mod(total, arg.int),
                else => {
                    std.debug.print("error: unknown operator {c}\n", .{op});
                    return InterpreterError.UnknownOperator;
                },
            }
        }
        return .{ .int = total };
    }

    fn binaryOp(self: *Interpreter, op: u8, args: []Ast) !Ast {
        var new_args = try self.allocator.alloc(Ast, args.len);
        for (args, 0..) |arg, i| {
            new_args[i] = try self.eval(arg);
        }
        switch (op) {
            '=' => {
                var total: u1 = 1;
                var prev = new_args[0];
                for (new_args[1..]) |arg| {
                    if (eqAst(prev, arg)) {
                        prev = arg;
                    } else {
                        total = 0;
                        break;
                    }
                }
                return .{ .int = total };
            },
            '>', '<', '}', '{' => {
                if (new_args[0] != .int) {
                    std.debug.print("error: expected integer, got {s}\n", .{try self.prettyStr(args[0])});
                    return InterpreterError.InvalidArgument;
                }
                var total: u1 = 1;
                var prev = new_args[0];
                for (new_args[1..]) |arg| {
                    if (arg != .int) {
                        std.debug.print("error: expected integer, got {s}\n", .{try self.prettyStr(arg)});
                        return InterpreterError.InvalidArgument;
                    }
                    switch (op) {
                        '>' => if (prev.int > arg.int) {
                            prev = arg;
                        } else {
                            total = 0;
                            break;
                        },
                        '<' => if (prev.int < arg.int) {
                            prev = arg;
                        } else {
                            total = 0;
                            break;
                        },
                        '}' => if (prev.int >= arg.int) {
                            prev = arg;
                        } else {
                            total = 0;
                            break;
                        },
                        '{' => if (prev.int <= arg.int) {
                            prev = arg;
                        } else {
                            total = 0;
                            break;
                        },
                        else => {
                            std.debug.print("error: unknown operator {c}\n", .{op});
                            return InterpreterError.UnknownOperator;
                        },
                    }
                }
                return .{ .int = total };
            },
            else => {
                std.debug.print("error: unknown operator {c}\n", .{op});
                return InterpreterError.UnknownOperator;
            },
        }
    }

    fn handleCall(self: *Interpreter, call: *parser.Call) !Ast {
        if (std.mem.eql(u8, call.name, "print")) {
            if (call.args.items.len == 0) {
                std.debug.print("\n", .{});
            }
            const len = call.args.items.len;
            for (call.args.items, 0..) |arg, i| {
                if (i == len - 1) {
                    std.debug.print("{s}\n", .{try self.prettyStr(try self.eval(arg))});
                } else {
                    std.debug.print("{s} ", .{try self.prettyStr(try self.eval(arg))});
                }
            }
            return .none;
        } else if (std.mem.eql(u8, call.name, "pretty")) {
            if (call.args.items.len != 1) {
                std.debug.print("error: `pretty` expects exactly one argument\n", .{});
                return InterpreterError.OneArgumentExpected;
            } else {
                return .{ .str = try self.prettyStr(call.args.items[0]) };
            }
        } else if (std.mem.eql(u8, call.name, "add")) {
            return try self.binaryOpInt('+', call.args.items);
        } else if (std.mem.eql(u8, call.name, "sub")) {
            return try self.binaryOpInt('-', call.args.items);
        } else if (std.mem.eql(u8, call.name, "mul")) {
            return try self.binaryOpInt('*', call.args.items);
        } else if (std.mem.eql(u8, call.name, "div")) {
            return try self.binaryOpInt('/', call.args.items);
        } else if (std.mem.eql(u8, call.name, "mod")) {
            return try self.binaryOpInt('%', call.args.items);
        } else if (std.mem.eql(u8, call.name, "eq")) {
            return try self.binaryOp('=', call.args.items);
        } else if (std.mem.eql(u8, call.name, "lt")) {
            return try self.binaryOp('<', call.args.items);
        } else if (std.mem.eql(u8, call.name, "gt")) {
            return try self.binaryOp('>', call.args.items);
        } else if (std.mem.eql(u8, call.name, "le")) {
            return try self.binaryOp('{', call.args.items);
        } else if (std.mem.eql(u8, call.name, "ge")) {
            return try self.binaryOp('}', call.args.items);
        } else if (std.mem.eql(u8, call.name, "if")) {
            if (call.args.items.len != 3) {
                std.debug.print("error: `if` expects three arguments, not {any}\n", .{call.args.items.len});
                return InterpreterError.ThreeArgumentsExpected;
            }
            const testing = try self.eval(call.args.items[0]);
            if (testing == .int) {
                if (testing.int != 0) {
                    return self.eval(call.args.items[1]);
                } else {
                    return self.eval(call.args.items[2]);
                }
            } else {
                std.debug.print("error: `if` expects an integer as the first argument\n", .{});
                return InterpreterError.InvalidArgument;
            }
        } else if (std.mem.eql(u8, call.name, "in")) {
            for (call.args.items) |*arg| {
                arg.* = try self.eval(arg.*);
            }
            const graph_idx = switch (call.args.items[0]) {
                .ugraph => call.args.items[0].ugraph.idx,
                .wgraph => call.args.items[0].wgraph.idx,
                .eugraph => call.args.items[0].eugraph.idx,
                .ewgraph => call.args.items[0].ewgraph.idx,
                else => {
                    std.debug.print("error: `in` expects a graph as the first argument\n", .{});
                    return InterpreterError.InvalidArgument;
                },
            };
            for (call.args.items[1..]) |arg| {
                for (graph_idx.items) |item| {
                    if (eqAst(item, arg)) {
                        break;
                    }
                } else {
                    return .{ .int = 0 };
                }
            }
            return .{ .int = 1 };
        } else if (std.mem.eql(u8, call.name, "addVx")) {
            // addVx(graph, vertex..) only extendable graphs
            if (call.args.items.len < 2) {
                std.debug.print("error: `addVx` expects at least two arguments\n", .{});
                return InterpreterError.LeastTwoArgumentsExpected;
            }
            for (call.args.items) |*arg| {
                arg.* = try self.eval(arg.*);
            }
            const graph = call.args.items[0];
            if (!isExtendableGraph(graph)) {
                std.debug.print("error: `addVx` expects an extendable graph as the first argument\n", .{});
                return InterpreterError.InvalidArgument;
            }
            for (call.args.items[1..]) |arg| {
                if (graph == .eugraph and for (graph.eugraph.idx.items) |item| {
                    if (eqAst(item, arg)) {
                        break false;
                    }
                } else true) {
                    try graph.eugraph.idx.append(arg);
                    try graph.eugraph.mat.append(std.ArrayList(bool).init(self.allocator));
                } else if (for (graph.ewgraph.idx.items) |item| {
                    if (eqAst(item, arg)) {
                        break false;
                    }
                } else true) {
                    try graph.ewgraph.idx.append(arg);
                    try graph.ewgraph.mat.append(std.ArrayList(Ast).init(self.allocator));
                }
            }
            return graph;
        } else if (std.mem.eql(u8, call.name, "addEu")) {
            // addEu(graph, vertex..) where edges are added between the each consecutive vertices, first vertex is the weight if the graph is weighted
            if (call.args.items.len < 3) {
                std.debug.print("error: `addEu` expects at least three arguments\n", .{});
                return InterpreterError.LeastThreeArgumentsExpected;
            }
            if (call.args.items.len < 4 and call.args.items[0] == .wgraph) {
                std.debug.print("error: `addEu` expects at least four arguments with weight\n", .{});
                return InterpreterError.LeastFourArgumentsExpected;
            }
            for (call.args.items) |*arg| {
                arg.* = try self.eval(arg.*);
            }
            const graph = call.args.items[0];
            const graph_idx = switch (call.args.items[0]) {
                .ugraph => call.args.items[0].ugraph.idx,
                .wgraph => call.args.items[0].wgraph.idx,
                else => {
                    std.debug.print("error: `addEu` expects a static graph as the first argument\n", .{});
                    return InterpreterError.InvalidArgument;
                },
            };

            const last = if (graph == .ugraph) call.args.items.len else call.args.items.len - 1;
            var argl = call.args.items[if (graph == .ugraph) 1 else 2];
            var argl_idx: usize = 0;
            if (for (graph_idx.items, 0..) |item, i| {
                if (eqAst(item, argl)) {
                    argl_idx = i;
                    break false;
                }
            } else true) {
                std.debug.print("error: vertex {s} not found in the graph\n", .{try self.prettyStr(argl)});
                return InterpreterError.VertexNotFound;
            }
            for (call.args.items[if (graph == .ugraph) 2 else 3..last]) |argr| {
                var argr_idx: usize = 0;
                if (for (graph_idx.items, 0..) |item, i| {
                    if (eqAst(item, argr)) {
                        argr_idx = i;
                        break false;
                    }
                } else true) {
                    std.debug.print("error: vertex {s} not found in the graph\n", .{try self.prettyStr(argr)});
                    return InterpreterError.VertexNotFound;
                }
                if (graph == .ugraph) {
                    if (graph.ugraph.dir) {
                        graph.ugraph.mat[argl_idx][argr_idx] = true;
                    } else {
                        graph.ugraph.mat[argl_idx][argr_idx] = true;
                        graph.ugraph.mat[argr_idx][argl_idx] = true;
                    }
                } else {
                    if (graph.wgraph.dir) {
                        graph.wgraph.mat[argl_idx][argr_idx] = call.args.items[1];
                    } else {
                        graph.wgraph.mat[argl_idx][argr_idx] = call.args.items[1];
                        graph.wgraph.mat[argr_idx][argl_idx] = call.args.items[1];
                    }
                }
                argl = argr;
                argl_idx = argr_idx;
            }
            return graph;
        } else {
            if (self.definitions.get(call.name)) |item| {
                if (item != .lamb) {
                    std.debug.print("error: {s} is not a function, but some value\n", .{call.name});
                    return InterpreterError.NotAFunction;
                }
                var new_args = try self.allocator.alloc(Ast, call.args.items.len);
                for (call.args.items, 0..) |arg, i| {
                    new_args[i] = try self.eval(arg);
                }
                return self.evalLamb(item.lamb, new_args);
            }
            std.debug.print("error: unknown function {s}\n", .{call.name});
            return InterpreterError.UnknownFunction;
        }
    }

    fn evalLamb(self: *Interpreter, lamb: *parser.Lambda, args: []Ast) !Ast {
        if (lamb.args.items.len != args.len) {
            std.debug.print("error: expected {d} arguments, got {d}\n", .{ lamb.args.items.len, args.len });
            return InterpreterError.UnexpectedArguments;
        }
        //try self.definitions.append(lamb.defs);
        //defer {
        //_ = self.definitions.pop();
        //}
        //const last = self.definitions.items.len - 1;
        for (lamb.args.items, args) |arg, val| {
            try self.definitions.put(arg, val);
        }
        return self.eval(lamb.code);
    }

    pub fn evalExpr(self: *Interpreter, expr: Ast) anyerror!Ast {
        switch (expr) {
            .lamb, .ugraph, .wgraph, .eugraph, .ewgraph, .int, .chr, .str, .mstr, .sym, .none => {
                return expr;
            },
            .block => {
                const block = expr.block;
                for (block.items, 0..) |item, i| {
                    if (isStmt(item)) {
                        try self.evalStmt(item);
                    } else {
                        if (i == block.items.len - 1) {
                            return self.evalExpr(item);
                        }
                        _ = try self.evalExpr(item);
                    }
                }
            },
            .id => {
                const id = expr.id;
                if (self.definitions.get(id)) |item| {
                    return self.eval(item);
                }
                std.debug.print("error: unknown identifier `{s}`\n", .{id});
                return InterpreterError.UnknownIdentifier;
            },
            .call => {
                const call = expr.call;
                return self.handleCall(call);
            },
            else => {
                std.debug.print("error: unexpected expression {any}\n", .{expr});
                return InterpreterError.UnexpectedExpression;
            },
        }
        return InterpreterError.UnknownError;
    }

    pub fn evalStmt(self: *Interpreter, stmt: Ast) !void {
        switch (stmt) {
            .def => {
                const def = stmt.def;
                //const last = self.definitions.items.len - 1;
                try self.definitions.put(def.name, def.value);
                if (def.value == .lamb) {
                    def.value.lamb.defs = self.definitions;
                }
            },
            .none => {},
            else => {
                std.debug.print("error: unexpected statement {any}\n", .{stmt});
            },
        }
    }

    pub fn evalNoReturn(self: *Interpreter, ast: Ast) anyerror!void {
        if (isStmt(ast)) {
            try self.evalStmt(ast);
        } else {
            _ = try self.evalExpr(ast);
        }
    }

    pub fn eval(self: *Interpreter, ast: Ast) anyerror!Ast {
        if (isStmt(ast)) {
            try self.evalStmt(ast);
            return .none;
        } else {
            return self.evalExpr(ast);
        }
    }

    pub fn interpret(self: *Interpreter, ast: std.ArrayList(Ast)) anyerror!void {
        for (ast.items) |item| {
            try self.evalNoReturn(item);
        }
    }
};
