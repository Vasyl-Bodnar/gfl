const std = @import("std");

pub fn Graph(comptime Mat: type) type {
    return struct {
        imm: bool,
        dir: bool,
        idx: std.ArrayList(Ast),
        mat: Mat,
    };
}

pub const UnweightedGraph = Graph([][]bool);
pub const WeightedGraph = Graph([][]Ast);
pub const ExtendableUnweightedGraph = Graph(std.ArrayList(std.ArrayList(bool)));
pub const ExtendableWeightedGraph = Graph(std.ArrayList(std.ArrayList(Ast)));
pub const Lambda = struct { defs: std.StringHashMap(Ast), args: std.ArrayList([]const u8), code: Ast };
pub const Def = struct { imm: bool, name: []const u8, value: Ast };
pub const Call = struct { name: []const u8, args: std.ArrayList(Ast) };

pub const Ast = union(enum) { int: i64, str: []const u8, mstr: []u8, chr: u8, id: []const u8, sym: []const u8, block: std.ArrayList(Ast), ugraph: *UnweightedGraph, wgraph: *WeightedGraph, eugraph: *ExtendableUnweightedGraph, ewgraph: *ExtendableWeightedGraph, lamb: *Lambda, def: *Def, call: *Call, none };

const ParseError = error{
    ImproperChar,
    ImproperId,
    ImproperMut,
    ImproperExtendable,
    ImproperWeightedGraph,
    UnfinishedChar,
    UnfinishedStr,
    UnfinishedSym,
    UnfinishedDef,
    UnfinishedArgs,
    UnfinishedFun,
    UnfinishedLamb,
    UnfinishedGraph,
    UnfinishedExtendable,
    UnfinishedWeighted,
    UnfinishedUnweighted,
    UnfinishedDirected,
    UnfinishedUndirected,
};

fn isint(char: u8) bool {
    return switch (char) {
        '0'...'9' => true,
        else => false,
    };
}

fn isspace(char: u8) bool {
    return switch (char) {
        ' ', '\n', '\r' => true,
        else => false,
    };
}

pub const Parser = struct {
    allocator: std.mem.Allocator,
    input: []const u8 = undefined,
    loc: usize = 0,
    output: std.ArrayList(Ast),

    pub fn init(allocator: std.mem.Allocator) Parser {
        return .{ .allocator = allocator, .output = std.ArrayList(Ast).init(allocator) };
    }

    pub fn parsePrint(parser: *Parser, input: []const u8, pretty: bool) !void {
        const parsed = try parser.parse(input);
        std.debug.print("Inputted \"{s}\":\n", .{input});
        for (parsed.items) |i| {
            if (pretty) {
                switch (i) {
                    .str => std.debug.print("\"{s}\"\n", .{i.str}),
                    .sym => std.debug.print("{s}\n", .{i.sym}),
                    .int => std.debug.print("{!}\n", .{i.int}),
                    .chr => std.debug.print("{c}\n", .{i.chr}),
                    .def => std.debug.print("{s}: {!} = {any}\n", .{ i.def.name, i.def.imm, i.def.value }),
                    .none => std.debug.print("none\n", .{}),
                    else => std.debug.print("{!}\n", .{i}),
                }
            } else {
                std.debug.print("{!}\n", .{i});
            }
        }
    }

    fn eof(self: Parser) bool {
        return self.loc >= self.input.len;
    }

    fn skipSpace(self: *Parser) void {
        while (!self.eof() and isspace(self.input[self.loc])) : (self.loc += 1) {}
    }

    fn parseInt(self: *Parser) Ast {
        var sum: i64 = 0;
        var sign: i64 = 1;
        if (self.input[self.loc] == '-') {
            self.loc += 1;
            sign = -1;
        }
        while (!self.eof() and isint(self.input[self.loc])) : (self.loc += 1) {
            sum = sum * 10 + self.input[self.loc] - 48;
        }
        return .{ .int = sum * sign };
    }

    fn parseChar(self: *Parser) !Ast {
        self.loc += 1;
        if (!self.eof()) {
            if (self.loc + 2 < self.input.len and !isspace(self.input[self.loc + 1])) {
                switch (self.input[self.loc]) {
                    's' => if (std.mem.eql(u8, self.input[self.loc .. self.loc + 5], "space")) {
                        self.loc += 5;
                        return .{ .chr = ' ' };
                    },
                    'n' => if (std.mem.eql(u8, self.input[self.loc .. self.loc + 7], "newline")) {
                        self.loc += 7;
                        return .{ .chr = '\n' };
                    },
                    else => return ParseError.ImproperChar,
                }
            } else {
                self.loc += 1;
                return .{ .chr = self.input[self.loc - 1] };
            }
        }
        return ParseError.UnfinishedChar;
    }

    fn parseStr(self: *Parser) !Ast {
        self.loc += 1;
        const stamp = self.loc;
        while (!self.eof() and self.input[self.loc] != '"') : (self.loc += 1) {}
        if (self.eof()) {
            return ParseError.UnfinishedStr;
        }
        const str = try self.allocator.alloc(u8, self.loc - stamp);
        std.mem.copyForwards(u8, str, self.input[stamp..self.loc]);
        self.loc += 1;
        return .{ .str = str };
    }

    fn parseSym(self: *Parser) !Ast {
        self.loc += 1;
        const stamp = self.loc;
        while (!self.eof()) : (self.loc += 1) {
            if (isspace(self.input[self.loc]) or self.input[self.loc] == '#' or self.input[self.loc] == '"' or self.input[self.loc] == '\'' or self.input[self.loc] == ':') {
                break;
            }
        }
        const sym = try self.allocator.alloc(u8, self.loc - stamp);
        std.mem.copyForwards(u8, sym, self.input[stamp..self.loc]);
        return .{ .sym = sym };
    }

    fn parseComm(self: *Parser) void {
        self.loc += 1;
        while (!self.eof() and self.input[self.loc] != '\n' and self.input[self.loc] != '\r') : (self.loc += 1) {}
    }

    fn parseId(self: *Parser) !Ast {
        const stamp = self.loc;
        while (!self.eof()) : (self.loc += 1) {
            if (isspace(self.input[self.loc]) or self.input[self.loc] == ')' or self.input[self.loc] == '#' or self.input[self.loc] == '"' or self.input[self.loc] == '\'' or self.input[self.loc] == '\\') {
                break;
            }
            if (self.input[self.loc] == '(') {
                const id = try self.allocator.alloc(u8, self.loc - stamp);
                std.mem.copyForwards(u8, id, self.input[stamp..self.loc]);
                return self.parseCall(id);
            }
            if (self.input[self.loc] == ':') {
                const id = try self.allocator.alloc(u8, self.loc - stamp);
                std.mem.copyForwards(u8, id, self.input[stamp..self.loc]);
                return self.parseDef(id);
            }
        }
        const id = try self.allocator.alloc(u8, self.loc - stamp);
        std.mem.copyForwards(u8, id, self.input[stamp..self.loc]);
        return .{ .id = id };
    }

    fn checkImm(self: *Parser) bool {
        switch (self.input[self.loc]) {
            'i' => if (self.loc + 3 < self.input.len and std.mem.eql(u8, self.input[self.loc .. self.loc + 3], "imm")) {
                self.loc += 3;
                return false;
            },
            'm' => if (self.loc + 3 < self.input.len and std.mem.eql(u8, self.input[self.loc .. self.loc + 3], "mut")) {
                self.loc += 3;
                return true;
            },
            else => return false,
        }
        return false;
    }

    fn parseDef(self: *Parser, name: []u8) !Ast {
        self.loc += 1;
        self.skipSpace();
        if (self.eof()) {
            return ParseError.UnfinishedDef;
        }
        const imm = self.checkImm();
        const expr = try self.parseExpr();
        const def = try self.allocator.create(Def);
        def.* = .{ .imm = imm, .name = name, .value = expr };
        return .{ .def = def };
    }

    fn collectArgsUntil(self: *Parser, last: u8) !std.ArrayList([]const u8) {
        var args = std.ArrayList([]const u8).init(self.allocator);
        while (!self.eof() and self.input[self.loc] != last) {
            const stamp = self.loc;
            while (!self.eof() and !isspace(self.input[self.loc]) and self.input[self.loc] != ')' and self.input[self.loc] != '#' and self.input[self.loc] != '"' and self.input[self.loc] != '\'' and self.input[self.loc] != '\\') : (self.loc += 1) {}
            const id = try self.allocator.alloc(u8, self.loc - stamp);
            std.mem.copyForwards(u8, id, self.input[stamp..self.loc]);
            try args.append(id);
            self.skipSpace();
        }
        if (self.eof()) {
            return ParseError.UnfinishedArgs;
        }
        return args;
    }

    fn parseLamb(self: *Parser) !Ast {
        self.loc += 1;
        const args = try self.collectArgsUntil('-');
        self.loc += 1;
        if (self.input[self.loc] != '>') {
            return ParseError.UnfinishedLamb;
        }
        self.loc += 1;
        const expr = try self.parseExpr();
        const lamb = try self.allocator.create(Lambda);
        lamb.* = .{ .defs = std.StringHashMap(Ast).init(self.allocator), .args = args, .code = expr };
        return .{ .lamb = lamb };
    }

    fn collectExprsUntil(self: *Parser, last: u8) !std.ArrayList(Ast) {
        var args = std.ArrayList(Ast).init(self.allocator);
        while (!self.eof() and self.input[self.loc] != last) {
            const expr = try self.parseExpr();
            try args.append(expr);
            self.skipSpace();
        }
        return args;
    }

    fn parseCall(self: *Parser, name: []const u8) !Ast {
        self.loc += 1;
        const args = try self.collectExprsUntil(')');
        self.loc += 1;
        const call = try self.allocator.create(Call);
        call.* = .{ .name = name, .args = args };
        return .{ .call = call };
    }

    fn parseBlock(self: *Parser) !Ast {
        self.loc += 1;
        const block = try self.collectExprsUntil(')');
        self.loc += 1;
        return .{ .block = block };
    }

    fn parseUndir(self: *Parser, weigh: bool) !struct { idx: std.ArrayList(Ast), lines: std.ArrayList(std.ArrayList(Ast)) } {
        self.loc += 1;
        if (self.eof()) {
            return ParseError.UnfinishedUndirected;
        }

        var lines = std.ArrayList(std.ArrayList(Ast)).init(self.allocator);
        var idx = std.ArrayList(Ast).init(self.allocator);

        while (!self.eof() and self.input[self.loc] != '}') {
            if (self.input[self.loc] == ',') {
                self.loc += 1;
                self.skipSpace();
                continue;
            }
            var block = std.ArrayList(Ast).init(self.allocator);
            while (!self.eof() and self.input[self.loc] != ',' and self.input[self.loc] != '}') {
                const expr = try self.parseExpr();
                if (weigh) {
                    if (block.items.len > 0) {
                        try idx.append(expr);
                    }
                } else {
                    try idx.append(expr);
                }
                try block.append(expr);
                self.skipSpace();
            }
            try lines.append(block);
        }
        self.loc += 1;

        return .{ .idx = idx, .lines = lines };
    }

    fn parseDir(self: *Parser, weigh: bool) !struct { idx: std.ArrayList(Ast), lines: std.ArrayList(std.ArrayList(Ast)) } {
        self.loc += 1;
        if (self.eof()) {
            return ParseError.UnfinishedDirected;
        }

        var lines = std.ArrayList(std.ArrayList(Ast)).init(self.allocator);
        var idx = std.ArrayList(Ast).init(self.allocator);

        while (!self.eof() and self.input[self.loc] != '>' and self.input[self.loc] != '<' and self.input[self.loc] != '|') {
            if (self.input[self.loc] == ',') {
                self.loc += 1;
                self.skipSpace();
                continue;
            }
            var block = std.ArrayList(Ast).init(self.allocator);
            while (!self.eof() and self.input[self.loc] != ',' and self.input[self.loc] != '>' and self.input[self.loc] != '<' and self.input[self.loc] != '|') {
                const expr = try self.parseExpr();
                if (weigh) {
                    if (block.items.len > 0) {
                        try idx.append(expr);
                    }
                } else {
                    try idx.append(expr);
                }
                try block.append(expr);
                self.skipSpace();
            }
            try lines.append(block);
        }
        self.loc += 1;

        return .{ .idx = idx, .lines = lines };
    }

    fn parseUndirWeighGraph(self: *Parser) !Ast {
        const undir = try self.parseUndir(true);
        const idx = undir.idx;
        const lines = undir.lines;

        const mat = try self.allocator.alloc([]Ast, idx.items.len);
        for (mat) |*pt| {
            pt.* = try self.allocator.alloc(Ast, idx.items.len);
        }

        for (lines.items) |block| {
            if (block.items.len < 3) {
                return ParseError.ImproperWeightedGraph;
            }
            const weight = block.items[0];
            var last = for (idx.items, 0..) |val, i| {
                if (std.meta.eql(block.items[1], val)) break i;
            } else @panic("Brok");
            for (block.items[2..]) |expr| {
                const next = for (idx.items, 0..) |val, i| {
                    if (std.meta.eql(expr, val)) break i;
                } else @panic("Brok");
                mat[last][next] = weight;
                mat[next][last] = weight;
                last = next;
            }
        }

        self.loc += 1;
        const graph = try self.allocator.create(WeightedGraph);
        graph.* = .{ .imm = true, .dir = false, .idx = idx, .mat = mat };
        return .{ .wgraph = graph };
    }

    fn parseDirWeighGraph(self: *Parser) !Ast {
        const dir = try self.parseDir(true);
        const idx = dir.idx;
        const lines = dir.lines;

        const mat = try self.allocator.alloc([]Ast, idx.items.len);
        for (mat) |*pt| {
            pt.* = try self.allocator.alloc(Ast, idx.items.len);
        }

        for (lines.items) |block| {
            if (block.items.len != 3) {
                return ParseError.ImproperWeightedGraph;
            }
            const weight = block.items[0];
            var last = for (idx.items, 0..) |val, i| {
                if (std.meta.eql(block.items[1], val)) break i;
            } else @panic("Brok");
            for (block.items[2..]) |expr| {
                const next = for (idx.items, 0..) |val, i| {
                    if (std.meta.eql(expr, val)) break i;
                } else @panic("Brok");
                if (!self.eof() and self.input[self.loc - 1] == '>') {
                    mat[last][next] = weight;
                } else if (!self.eof() and self.input[self.loc - 1] == '<') {
                    mat[next][last] = weight;
                } else {
                    mat[last][next] = weight;
                    mat[next][last] = weight;
                }
                last = next;
            }
        }

        const graph = try self.allocator.create(WeightedGraph);
        graph.* = .{ .imm = true, .dir = true, .idx = idx, .mat = mat };
        return .{ .wgraph = graph };
    }

    fn parseUndirUnweighGraph(self: *Parser) !Ast {
        const undir = try self.parseUndir(false);
        const idx = undir.idx;
        const lines = undir.lines;

        const mat = try self.allocator.alloc([]bool, idx.items.len);
        for (mat) |*pt| {
            pt.* = try self.allocator.alloc(bool, idx.items.len);
        }

        for (lines.items) |block| {
            var last = for (idx.items, 0..) |val, i| {
                if (std.meta.eql(block.items[0], val)) break i;
            } else @panic("Brok");
            for (block.items[1..]) |expr| {
                const next = for (idx.items, 0..) |val, i| {
                    if (std.meta.eql(expr, val)) break i;
                } else @panic("Brok");
                mat[last][next] = true;
                mat[next][last] = true;
                last = next;
            }
        }

        const graph = try self.allocator.create(UnweightedGraph);
        graph.* = .{ .imm = true, .dir = false, .idx = idx, .mat = mat };
        return .{ .ugraph = graph };
    }

    fn parseDirUnweighGraph(self: *Parser) !Ast {
        const dir = try self.parseDir(false);
        const idx = dir.idx;
        const lines = dir.lines;

        const mat = try self.allocator.alloc([]bool, idx.items.len);
        for (mat) |*pt| {
            pt.* = try self.allocator.alloc(bool, idx.items.len);
        }

        for (lines.items) |block| {
            var last = for (idx.items, 0..) |val, i| {
                if (std.meta.eql(block.items[1], val)) break i;
            } else @panic("Brok");
            for (block.items[2..]) |expr| {
                const next = for (idx.items, 0..) |val, i| {
                    if (std.meta.eql(expr, val)) break i;
                } else @panic("Brok");
                if (self.input[self.loc - 1] == '>') {
                    mat[last][next] = true;
                } else if (self.input[self.loc - 1] == '<') {
                    mat[next][last] = true;
                } else {
                    mat[last][next] = true;
                    mat[next][last] = true;
                }
                last = next;
            }
        }

        const graph = try self.allocator.create(UnweightedGraph);
        graph.* = .{ .imm = true, .dir = true, .idx = idx, .mat = mat };
        return .{ .ugraph = graph };
    }

    fn parseExtUndirWeighGraph(self: *Parser) !Ast {
        const undir = try self.parseUndir(true);
        const idx = undir.idx;
        const lines = undir.lines;

        var mat = try std.ArrayList(std.ArrayList(Ast)).initCapacity(self.allocator, idx.items.len);
        mat.items.len = idx.items.len;
        for (mat.items) |*pt| {
            pt.* = try std.ArrayList(Ast).initCapacity(self.allocator, idx.items.len);
        }

        for (lines.items) |block| {
            if (block.items.len != 3) {
                return ParseError.ImproperWeightedGraph;
            }
            const weight = block.items[0];
            var last = for (idx.items, 0..) |val, i| {
                if (std.meta.eql(block.items[1], val)) break i;
            } else @panic("Brok");
            for (block.items[2..]) |expr| {
                const next = for (idx.items, 0..) |val, i| {
                    if (std.meta.eql(expr, val)) break i;
                } else @panic("Brok");
                mat.items[last].allocatedSlice()[next] = weight;
                mat.items[next].allocatedSlice()[last] = weight;
                last = next;
            }
        }

        self.loc += 1;
        const graph = try self.allocator.create(ExtendableWeightedGraph);
        graph.* = .{ .imm = true, .dir = false, .idx = idx, .mat = mat };
        return .{ .ewgraph = graph };
    }

    fn parseExtDirWeighGraph(self: *Parser) !Ast {
        const dir = try self.parseDir(true);
        const idx = dir.idx;
        const lines = dir.lines;

        var mat = try std.ArrayList(std.ArrayList(Ast)).initCapacity(self.allocator, idx.items.len);
        mat.items.len = idx.items.len;
        for (mat.items) |*pt| {
            pt.* = try std.ArrayList(Ast).initCapacity(self.allocator, idx.items.len);
        }

        for (lines.items) |block| {
            if (block.items.len != 3) {
                return ParseError.ImproperWeightedGraph;
            }
            const weight = block.items[0];
            var last = for (idx.items, 0..) |val, i| {
                if (std.meta.eql(block.items[1], val)) break i;
            } else @panic("Brok");
            for (block.items[2..]) |expr| {
                const next = for (idx.items, 0..) |val, i| {
                    if (std.meta.eql(expr, val)) break i;
                } else @panic("Brok");
                if (self.input[self.loc - 1] == '>') {
                    mat.items[last].allocatedSlice()[next] = weight;
                } else if (self.input[self.loc - 1] == '<') {
                    mat.items[next].allocatedSlice()[last] = weight;
                } else {
                    mat.items[last].allocatedSlice()[next] = weight;
                    mat.items[next].allocatedSlice()[last] = weight;
                }
                last = next;
            }
        }

        const graph = try self.allocator.create(ExtendableWeightedGraph);
        graph.* = .{ .imm = true, .dir = true, .idx = idx, .mat = mat };
        return .{ .ewgraph = graph };
    }

    fn parseExtUndirUnweighGraph(self: *Parser) !Ast {
        const undir = try self.parseUndir(false);
        const idx = undir.idx;
        const lines = undir.lines;

        var mat = try std.ArrayList(std.ArrayList(bool)).initCapacity(self.allocator, idx.items.len);
        mat.items.len = idx.items.len;
        for (mat.items) |*pt| {
            pt.* = try std.ArrayList(bool).initCapacity(self.allocator, idx.items.len);
        }

        for (lines.items) |block| {
            var last = for (idx.items, 0..) |val, i| {
                if (std.meta.eql(block.items[0], val)) break i;
            } else @panic("Brok");
            for (block.items[1..]) |expr| {
                const next = for (idx.items, 0..) |val, i| {
                    if (std.meta.eql(expr, val)) break i;
                } else @panic("Brok");
                mat.items[last].allocatedSlice()[next] = true;
                mat.items[next].allocatedSlice()[last] = true;
                last = next;
            }
        }

        const graph = try self.allocator.create(ExtendableUnweightedGraph);
        graph.* = .{ .imm = true, .dir = false, .idx = idx, .mat = mat };
        return .{ .eugraph = graph };
    }

    fn parseExtDirUnweighGraph(self: *Parser) !Ast {
        const dir = try self.parseDir(false);
        const idx = dir.idx;
        const lines = dir.lines;

        var mat = try std.ArrayList(std.ArrayList(bool)).initCapacity(self.allocator, idx.items.len);
        mat.items.len = idx.items.len;
        for (mat.items) |*pt| {
            pt.* = try std.ArrayList(bool).initCapacity(self.allocator, idx.items.len);
        }

        for (lines.items) |block| {
            var last = for (idx.items, 0..) |val, i| {
                if (std.meta.eql(block.items[1], val)) break i;
            } else @panic("Brok");
            for (block.items[2..]) |expr| {
                const next = for (idx.items, 0..) |val, i| {
                    if (std.meta.eql(expr, val)) break i;
                } else @panic("Brok");
                if (self.input[self.loc - 1] == '>') {
                    mat.items[last].allocatedSlice()[next] = true;
                } else if (self.input[self.loc - 1] == '<') {
                    mat.items[next].allocatedSlice()[last] = true;
                } else {
                    mat.items[last].allocatedSlice()[next] = true;
                    mat.items[next].allocatedSlice()[last] = true;
                }
                last = next;
            }
        }

        const graph = try self.allocator.create(ExtendableUnweightedGraph);
        graph.* = .{ .imm = true, .dir = true, .idx = idx, .mat = mat };
        return .{ .eugraph = graph };
    }

    fn parseWeigh(self: *Parser) !Ast {
        self.loc += 1;
        if (!self.eof() and self.input[self.loc] == '*') {
            self.loc += 1;
            if (!self.eof() and self.input[self.loc] == '{') {
                return self.parseExtUndirWeighGraph();
            } else if (!self.eof() and self.input[self.loc] == '|') {
                return self.parseExtDirWeighGraph();
            } else {
                return ParseError.ImproperWeightedGraph;
            }
        } else if (!self.eof() and self.input[self.loc] == '{') {
            return self.parseUndirWeighGraph();
        } else if (!self.eof() and self.input[self.loc] == '|') {
            return self.parseDirWeighGraph();
        } else {
            return ParseError.ImproperWeightedGraph;
        }
    }

    fn parseExtendable(self: *Parser) !Ast {
        self.loc += 1;
        if (!self.eof() and self.input[self.loc] == '|') {
            return self.parseExtDirUnweighGraph();
        } else if (!self.eof() and self.input[self.loc] == '{') {
            return self.parseExtUndirUnweighGraph();
        } else {
            return ParseError.ImproperExtendable;
        }
    }

    fn parseMut(self: *Parser) !Ast {
        self.loc += 1;
        if (!self.eof() and self.input[self.loc] == '"') {
            var str = try self.parseStr();
            return .{ .mstr = @constCast(str.str) };
        } else if (!self.eof() and self.input[self.loc] == '@') {
            var weigh = try self.parseWeigh();
            switch (weigh) {
                .wgraph => weigh.wgraph.*.imm = false,
                .ewgraph => weigh.ewgraph.*.imm = false,
                else => return ParseError.ImproperMut,
            }
            return weigh;
        } else if (!self.eof() and self.input[self.loc] == '*') {
            var ext = try self.parseExtendable();
            switch (ext) {
                .eugraph => ext.eugraph.*.imm = false,
                .ewgraph => ext.ewgraph.*.imm = false,
                else => return ParseError.ImproperMut,
            }
            return ext;
        } else if (!self.eof() and self.input[self.loc] == '{') {
            var undir = try self.parseUndirUnweighGraph();
            switch (undir) {
                .ugraph => undir.ugraph.*.imm = false,
                .eugraph => undir.eugraph.*.imm = false,
                else => return ParseError.ImproperMut,
            }
            return undir;
        } else if (!self.eof() and self.input[self.loc] == '|') {
            var dir = try self.parseDirUnweighGraph();
            switch (dir) {
                .ugraph => dir.ugraph.*.imm = false,
                .eugraph => dir.eugraph.*.imm = false,
                else => return ParseError.ImproperMut,
            }
            return dir;
        } else {
            return ParseError.ImproperMut;
        }
    }

    fn parseExpr(self: *Parser) anyerror!Ast {
        return switch (self.input[self.loc]) {
            '-', '0'...'9' => self.parseInt(),
            '\'' => self.parseChar(),
            '\\' => self.parseLamb(),
            '"' => self.parseStr(),
            '`' => self.parseSym(),
            '(' => self.parseBlock(),
            '!' => self.parseMut(),
            '*' => self.parseExtendable(),
            '@' => self.parseWeigh(),
            '{' => self.parseUndirUnweighGraph(),
            '|' => self.parseDirUnweighGraph(),
            '#' => {
                self.parseComm();
                if (self.eof()) {
                    return .none;
                }
                return self.parseExpr();
            },
            ' ', '\n', '\r' => {
                self.loc += 1;
                if (self.eof()) {
                    return .none;
                }
                return self.parseExpr();
            },
            else => self.parseId(),
        };
    }

    pub fn parse(self: *Parser, input: []const u8) !std.ArrayList(Ast) {
        self.loc = 0;
        self.input = input;
        self.output = std.ArrayList(Ast).init(self.allocator);
        return self.parseInner();
    }

    fn parseInner(self: *Parser) !std.ArrayList(Ast) {
        if (self.eof()) {
            return self.output;
        }
        const expr = try self.parseExpr();
        switch (expr) {
            .none => {},
            else => try self.output.append(expr),
        }
        return self.parseInner();
    }
};
