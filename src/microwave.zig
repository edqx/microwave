const std = @import("std");

pub const Scanner = @import("Scanner.zig");
pub const Parser = @import("Parser.zig");
pub const Populate = @import("populate.zig").Populate;
pub const WriteStream = @import("WriteStream.zig");
pub const stringify = @import("stringify.zig");

pub const Stringify = stringify.Stringify;

pub const DateTime = @import("rfc3339.zig").DateTime;

pub const DocumentLeaky = struct {
    key_set: std.StringHashMapUnmanaged(void),
    table: Parser.Value.Table,

    pub fn deinit(document: DocumentLeaky, allocator: std.mem.Allocator) void {
        var value: Parser.Value = .{ .table = document.table };
        value.deinitDeep(allocator);

        Parser.deinitKeySet(allocator, document.key_set);
    }
};

pub const Document = struct {
    arena: std.heap.ArenaAllocator,
    table: Parser.Value.Table,

    pub fn deinit(document: Document) void {
        document.arena.deinit();
    }
};

pub fn parseFromScannerLeaky(gpa: std.mem.Allocator, scanner: *Scanner) !DocumentLeaky {
    var parser: Parser = .{
        .allocator = gpa,
        .key_allocator = gpa,
        .scanner = scanner,
    };
    errdefer parser.deinit();

    const document_table = try parser.takeDocumentTable();
    return .{
        .key_set = parser.key_set,
        .table = document_table,
    };
}

pub fn parseFromScanner(gpa: std.mem.Allocator, scanner: *Scanner) !Document {
    var arena: std.heap.ArenaAllocator = .init(gpa);
    errdefer arena.deinit();

    const leaky = try parseFromScannerLeaky(arena.allocator(), scanner);
    return .{
        .arena = arena,
        .table = leaky.table,
    };
}

pub fn parseFromReaderLeaky(gpa: std.mem.Allocator, reader: *std.Io.Reader) !DocumentLeaky {
    var scanner: Scanner = .{ .reader = reader };
    return try parseFromScannerLeaky(gpa, &scanner);
}

pub fn parseFromReader(gpa: std.mem.Allocator, reader: *std.Io.Reader) !Document {
    var scanner: Scanner = .{ .reader = reader };
    return try parseFromScanner(gpa, &scanner);
}

pub fn parseFromSliceLeaky(gpa: std.mem.Allocator, slice: []const u8) !DocumentLeaky {
    var reader: std.Io.Reader = .fixed(slice);
    return try parseFromReaderLeaky(gpa, &reader);
}

pub fn parseFromSlice(gpa: std.mem.Allocator, slice: []const u8) !Document {
    var reader: std.Io.Reader = .fixed(slice);
    return try parseFromReader(gpa, &reader);
}

test {
    _ = Scanner;
    _ = Parser;
    _ = Populate;
    _ = WriteStream;
    _ = stringify;
}
