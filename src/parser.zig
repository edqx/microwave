const std = @import("std");

const Scanner = @import("Scanner.zig");

pub const Value = union(enum) {
    pub const Table = std.StringHashMapUnmanaged(Value);

    pub const DateTime = struct {
        date: ?[]const u8,
        time: ?[]const u8,
        offset: ?[]const u8,
    };

    table: Table,
    array: std.ArrayListUnmanaged(Value),
    array_of_tables: std.ArrayListUnmanaged(Table),
    string: []const u8,
    integer: usize,
    string_integer: []const u8,
    float: f64,
    boolean: bool,
    date_time: DateTime,
};

pub fn Parser(ScannerType: type) type {
    return struct {
        const ParserT = @This();

        allocator: std.mem.Allocator,
        scanner: ScannerType,

        pub fn parseIntoTable(self: ParserT, table: *Value.Table) !void {
            var next_token: ?Scanner.Token = try self.scanner.next() orelse return;
            while (next_token) |token| {
                switch (token.kind) {
                    .ignored => {
                        next_token = try self.scanner.next() orelse return;
                        continue;
                    },
                    .key => {},
                }
            }
        }
    };
}

allocator: std.mem.Allocator,
