const std = @import("std");
const parse = @import("parser.zig");
const interp = @import("interpreter.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) std.debug.print("Leak Detected!", .{});
    }

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);

    if (args.len != 2) {
        std.debug.print("Usage: {s} <filename>\n", .{args[0]});
    } else if (args.len == 2) {
        const filename = args[1];

        const file = try std.fs.cwd().openFile(filename, .{});
        defer file.close();

        const stat = try file.stat();

        const bytes = try file.readToEndAlloc(allocator, stat.size);

        var parser = parse.Parser.init(allocator);

        var result = try parser.parse(bytes);

        var interpreter = try interp.Interpreter.init(allocator);

        try interpreter.interpret(result);
    }
}

test "basic strings" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) std.debug.print("Leak Detected!", .{});
    }

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    var parser = parse.Parser.init(allocator);

    try parser.parsePrint("123\n234\n513", false);
    try parser.parsePrint("'h 'A 'space ", false);
    try parser.parsePrint("\"hello\" \"Hello World!\" ", false);
    try parser.parsePrint("!\"hello\" !\"Hello World!\" ", false);
    try parser.parsePrint("`hello ##Heya Friend", false);
    try parser.parsePrint("x: imm `sum", true);
    try parser.parsePrint("x: \\x y -> sum", true);
    try parser.parsePrint("x(1 2)", false);
    try parser.parsePrint("+(x(1 2) 12)", false);
    try parser.parsePrint("(1 2 3)", false);
    try parser.parsePrint(
        \\int: 134
        \\str: "Bonjour"
        \\int: 'space
        \\int: `sup
        \\lam: \a b -> (
        \\     q()
        \\     +(a b)
        \\     ) # lambda
        \\printfmt("{}" lam(1 2)) # prints 3
    , false);
    try parser.parsePrint("{1 2, 3 4, 5 6}", false);
    try parser.parsePrint("|1 2, 3 4, 5 6<", false);
    try parser.parsePrint("@{1 1 2, 3 3 4, `hey 5 6}", false);
    try parser.parsePrint("@|1 1 2, 3 3 4, `hey 5 6>", false);
    try parser.parsePrint("@*{1 1 2, 3 3 4, `hey 5 6}", false);
    try parser.parsePrint("!*{1 1 2, 3 3 4, `hey 5 6}", false);
    try parser.parsePrint("hello: @|1 2 5, 2 1 10, 43 'c 15>", false);
    try parser.parsePrint("!@*|1 1 2, 3 3 4, `hey 5 6<", false);
}
