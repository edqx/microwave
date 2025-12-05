//! Root namespace for **microwave**, a largely spec-compliant TOML parser.
//!
//! There are three main APIs for interfacing effectively with TOML files:
//! - `Parser` - for parsing raw TOML files
//! - `Populate` - for populating Zig types with TOML values
//! - `Stringify` - for converting Zig types into a TOML document
//!
//! Also, the `Scanner` API can be used for lower level stateless lexing of TOML files,
//! and the `WriteStream` API can be used for lower level building of TOML files.

const std = @import("std");

pub const Scanner = @import("Scanner.zig");
pub const Parser = @import("Parser.zig");
pub const Populate = @import("populate.zig").Populate;
pub const WriteStream = @import("WriteStream.zig");
pub const Stringify = @import("Stringify.zig");

pub const DateTime = @import("rfc3339.zig").DateTime;

/// Represents an unmanaged parsed TOML document with potentially leaky data.
pub const DocumentLeaky = struct {
    /// A set of distinct [interned](https://en.wikipedia.org/wiki/String_interning) keys
    /// used in tables.
    ///
    /// Can be de-initialized manually with `Parser.deinitKeySet`.
    key_set: std.StringHashMapUnmanaged(void),
    /// The root document table.
    table: Parser.Value.Table,

    /// De-initialize all data kept by this document, including keys used by tables.
    ///
    /// This is not guaranteed to be a fully safe and leak-free operation, but in most tested
    /// cases will be.
    pub fn deinit(document: DocumentLeaky, allocator: std.mem.Allocator) void {
        var value: Parser.Value = .{ .table = document.table };
        value.deinitDeep(allocator);

        Parser.deinitKeySet(allocator, document.key_set);
    }
};

/// Represents a parsed TOML document with fully leak-safe data through the use of an
/// `std.heap.ArenaAllocator`.
pub const Document = struct {
    /// The internal arena used to allocate all value data and table keys.
    arena: std.heap.ArenaAllocator,
    /// The root document table.
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
    _ = Stringify;
}
