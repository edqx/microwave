# Microwave

A TOML parser for [Zig](https://ziglang.org).

This parser should be spec compliant. (WIP: 410/557 of https://github.com/toml-lang/toml-test/)

## Features
- [x] Parse all spec-compliant TOML documents. (WIP)
- [x] Use Zig readers and writers
- [x] Populate structs
- [x] Populate dynamic values
- [x] TOML builder/write stream
- [x] Stringify entire structs and tables

## Usage

Microwave has 5 sets of APIs:
- [Parser API](#parser-api) - for parsing an entire TOML file into a tree-like structure
- [Populate API](#populate-api) - for mapping a TOML file into a given struct
- [Stringify API](#stringify-api) - for writing TOML files from a tree or given struct
- [Write Stream API](#write-stream-api) - for building TOML files safely with debug assertions
- [Tokeniser/Scanner API](#scanner-api) - for iterating through a TOML file for each significant token

### Parser API
Microwave allows you to parse an entire TOML file from either a slice or reader
into a tree-like structure that can be traversed, inspected or modified manually.

#### From Slice
```zig
const document = try microwave.parse.fromSlice(allocator, toml_text);
defer document.deinit(); // all pointers will be freed using the created internal arena

// use document.root_table
```

#### From Reader
```zig
const document = try microwave.parse.fromReader(allocator, file.reader());
defer document.deinit();

// use document.root_table
```

#### Owned Allocations API
If you would like to personally own all of the pointers without creating an arena
for them, use the `*Owned` variation of the functions.

These return a `parse.Value.Table` directly, representing the root table of the
TOML file.

The best way to free the resulting root table is to use `parse.deinitTable`.

```zig
var owned_tree = try microwave.parse.fromSliceOwned(allocator, toml_text); // or .fromReaderOwned
defer microwave.parse.deinitTable(allocator, &owned_tree);

// use owned_tree
```

#### Value API
```zig
pub const Value = union(enum) {
    pub const Table = std.StringArrayHashMapUnmanaged(Value);
    pub const Array = std.ArrayListUnmanaged(Value);
    pub const ArrayOfTables = std.ArrayListUnmanaged(Table);

    pub const DateTime = struct {
        date: ?[]const u8 = null,
        time: ?[]const u8 = null,
        offset: ?[]const u8 = null,

        pub fn dupe(self: DateTime, allocator: std.mem.Allocator) !DateTime;
        pub fn deinit(self: DateTime, allocator: std.mem.Allocator) ;
    };

    none: void,
    table: Table,
    array: Array,
    array_of_tables: ArrayOfTables,
    string: []const u8,
    integer: i64,
    float: f64,
    boolean: bool,
    date_time: DateTime,

    pub fn dupeRecursive(self: Value, allocator: std.mem.Allocator) !Value;
    pub fn deinitRecursive(self: *Value, allocator: std.mem.Allocator) void;
};
```

### Populate API
It's often helpful to map a TOML file directly onto a Zig struct, for example for
config files. Microwave lets you do this using the `Populate(T)` API:

```zig
const Dog = struct {
    pub const Friend = struct {
        name: []const u8,
    };

    name: []const u8,
    cross_breeds: []const []const u8,
    age: i64,

    friends: []Friend,
    
    vet_info: microwave.parse.Value.Table,
}

const dog = try microwave.Populate(Dog).createFromSlice(allocator, toml_text); // or .createFromReader
defer dog.deinit();
```

#### Struct Shape
Since TOML only supports a subset of the types that are available in Zig, your destination
struct must consist of the following types:

| TOML Type | Zig Type | Examples |
|-|-|-|
| String | `[]const u8` | `"Barney"` |
| Float | `f64` | `5.0e+2` |
| Integer | `i64`, `f64` | `16` |
| Boolean | `bool` | `true`, `false` |
| Date/Time | `parse.Value.DateTime` | `2025-04-19T00:43:00.500+05:00` |
| Specific Table | `struct { ... }` | `{ name = "Barney", age = 16 }` |
| Array of Tables | `[]struct {}` | `[[pet]]` |
| Inline Array | `[]T` | `["Hello", "Bonjour", "Hola"]` |
| Any Table | `parse.Value.Table` | Any TOML table |
| Any Value | `parse.Value` | Any TOML value |

You can also specify an option of different types using unions. For example:

```zig
const Animal = union(enum) {
    dog: struct {
        name: []const u8,
        breed: []const u8,
    },
    cat: struct {
        name: []const u8,
        number_of_colours: usize,
    },
};

const animal = try microwave.Populate(Animal).createFromSlice(allocator, toml_text);
defer animal.deinit();
```

If the field is entirely optional and may not exist, use the Zig optional indiciator
on the type, for example:

```zig
const Person = struct {
    name: []const u8,
    age: i64,
    salary: f64,
    job: ?[]const u8, // can be missing from the TOML file
};

const person = try microwave.Populate(Person).createFromSlice(allocator, toml_text);
defer person.deinit();
```

#### Owned Allocations API
Like the parser API, you might want to own the pointers yourself rather than delegate
them to an arena. You can use the `*Owned` variations of the functions.

These return the value directly.

You can free the data in the returned value however you want, but if you're using
an stack-based allocator like arena or fixed buffer allocator, then it's best to
use `Populate(T).deinitRecursive`.

```zig
var dog = try microwave.Populate(Dog).createFromSliceOwned(allocator, toml_text);
defer microwave.Populate(Dog).deinitRecursive(allocator, &dog);
```

#### 'Into' API
Instead of making Microwave create the value to populate, you can provide it with
a pointer to an existing one to populate using the `into*` functions:

```zig
var dog: Dog = undefined;
try microwave.Populate(Dog).intoFromSliceOwned(allocator, &dog); // or .intoFromReaderOwned
defer microwave.Populate(Dog).deinitRecursive(allocator, &dog);
```

### Stringify API
Microwave can try its best to serialise a given struct value or `parse.Value.Table`
into a writer:

```zig
try microwave.stringify.write(allocator, dog, file.writer());
```

```zig
try microwave.stringify.writeTable(allocator, root_table, file.writer());
```

> [!NOTE]
> There's no need to de-init anything, the allocator is for temporary allocations.

### Write Stream API
You can build a TOML file manually, with safety assertions that the file is well-formed,
using the write stream API:

```zig
var stream: microwave.write_stream.Stream(@TypeOf(file.writer()), .{
    .newlines = .lf,
    unicode_full_escape_strings = false,
    format_float_options = .{
        .mode = .scientific,
        .precision = null,
    },
    date_time_separator = .t,
}) = .{
    .underlying_writer = file.writer(),
    .allocator = allocator,
};
defer stream.deinit();
```

You can use the following functions on the `write_stream.Stream` struct to build your TOML file:
```zig
pub fn beginDeepKeyPair(self: *Stream, key_parts: []const []const u8) !void;
pub fn beginKeyPair(self: *Stream, key_name: []const u8) !void;

pub fn writeString(self: *Stream, string: []const u8) !void;
pub fn writeInteger(self: *Stream, integer: i64) !void;
pub fn writeFloat(self: *Stream, float: f64) !void;
pub fn writeBoolean(self: *Stream, boolean: bool) !void;
pub fn writeDateTime(self: *Stream, date_time: parse.Value.DateTime) !void;

pub fn beginArray(self: *Stream) !void;
pub fn arrayLine(self: *Stream) !void;
pub fn endArray(self: *Stream) !void;

pub fn beginInlineTable(self: *Stream) !void;
pub fn endInlineTable(self: *Stream) !void;

pub fn writeDeepTable(self: *Stream, key_parts: []const []const u8) !void;
pub fn writeTable(self: *Stream, key_name: []const u8) !void;

pub fn writeDeepManyTable(self: *Stream, key_parts: []const []const u8) !void;
pub fn writeManyTable(self: *Stream, key_name: []const u8) !void;
```

### Scanner API
As a low level API, Microwave also provides the ability to scan through a file and
iterate through individual tokens.

No safety checks or file state is kept at this stage, so it doesn't guarantee a
well-formed TOML file. Those checks are done in the [parsing stage](#parser-api).

#### Whole Slice Scanner
If you have access to the entire slice of the TOML file, you can initialise
the scanner directly:
```zig
var scanner: microwave.Scanner = .{ .buffer = slice };

while (try scanner.next()) |token| {
    // token.kind, token.range.start, token.range.end
}
```

The default scanner may return any of the following errors:
```zig
pub const Error = error{ UnexpectedEndOfBuffer, UnexpectedByte };
```

### Buffered Reader Scanner
You can also tokenise the TOML file using a reader:
```zig
var scanner = microwave.Scanner.bufferedReaderScanner(file.reader());

// use scanner.next() in the same way
```

The buffered reader scanner may return any of the following errors:
```zig
pub const Error = error{ UnexpectedEndOfBuffer, UnexpectedByte, BufferTooSmall };
```

#### Handling Errors

When encountering an error, you can use `scanner.cursor()` to get
the file offset that it occurred at.

If you encounter `error.BufferTooSmall` while using the buffered reader scanner,
you can increase the size of the buffer for your project by instantiating `Scanner.BufferedReaderScanner`
directly:
```zig
var scanner = microwave.Scanner.BufferedReaderScanner(8192, @TypeOf(file.reader())) = .{
    .reader = file.reader(),
};
```

#### Token Contents
To access the contents of a token, you can use the `scanner.tokenContents` function:
```zig
while (try scanner.next()) |token| {
    if (token.kind == .string) {
        std.log.info("Found string! {s}", .{ scanner.tokenContents(token) });
    }
}
```

> [!NOTE]
> For the buffered reader scanner, previous token contents may be invalidated at any
> point while iterating.

## Related Projects
Check out my other project, [dishwasher](https://github.com/edqx/dishwasher) for parsing XML files.

## Why 'Microwave'?
Not sure.

## License
All microwave code is under the MIT license.