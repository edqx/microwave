# Microwave

A TOML parser for [Zig](https://ziglang.org).

This parser should be spec compliant.

## Features
- [x] Parse all spec-compliant TOML documents.

> [!WARNING]
> WIP: See [Spec Compliancy](#spec-compliancy)

- [x] Use Zig readers and writers
- [x] Populate structs
- [x] Populate dynamic values
- [x] TOML builder/write stream
- [x] Stringify entire structs and tables

### Documentation
Run `zig build docs` to access local documentation for Microwave. Use your favourite method to serve static sites,
for example with Python: `python -m http.server` or with NodeJS: `npx serve`.

### Scanner API
As a low level API, Microwave provides the ability to scan through a file and
iterate through individual tokens.

It doesn't guarantee a well-formed TOML file. Most of those checks are done in the
[parsing stage](#parser-api).

#### Initialise Scanner
To initialise the scanner, you need only pass in a [`*Reader`](https://ziglang.org/documentation/0.15.2/std/#std.Io.Reader)
of your choice:
```zig
var scanner: microwave.Scanner = .{ reader = &reader };

while (try scanner.next()) |token| {
    // token.kind, token.contents
```

> [!IMPORTANT]
> A decently sized buffer is important for properly parsing many TOML structures. 35 bytes is the most
> required, which is the maximum size of a [date](https://toml.io/en/v1.0.0#offset-date-time) value.

> [!WARNING]
> Previous token contents may be invalidated at any point while iterating, depending on your reader
> buffer size.

## Related Projects
Check out my other project, [dishwasher](https://github.com/edqx/dishwasher) for parsing XML files.

## Why 'Microwave'?
Not sure.

## Spec Compliancy
See the [tests](https://github.com/edqx/microwave/tree/master/tests) folder to check
Microwave against the various official TOML test cases.

The test suite is downloaded directly from the [official TOML test suite repository](https://github.com/toml-lang/toml-test).

```
- fail: invalid/control/bare-cr.toml
- fail: invalid/control/bare-null.toml
- fail: invalid/control/comment-cr.toml
- fail: invalid/control/comment-del.toml
- fail: invalid/control/comment-ff.toml
- fail: invalid/control/comment-lf.toml
- fail: invalid/control/comment-null.toml
- fail: invalid/control/comment-us.toml
- fail: invalid/encoding/bad-codepoint.toml
- fail: invalid/encoding/bad-utf8-at-end.toml
- fail: invalid/encoding/bad-utf8-in-comment.toml
- fail: invalid/encoding/bad-utf8-in-multiline-literal.toml
- fail: invalid/encoding/bad-utf8-in-string-literal.toml
- fail: invalid/encoding/utf16-bom.toml
- fail: invalid/encoding/utf16-comment.toml
- fail: invalid/float/exp-point-2.toml
- fail: invalid/float/exp-point-3.toml
- fail: invalid/float/leading-point-neg.toml
- fail: invalid/float/leading-point-plus.toml
- fail: invalid/float/trailing-point.toml
- fail: invalid/float/trailing-point-min.toml
- fail: invalid/float/trailing-point-plus.toml
- fail: invalid/key/after-array.toml
- fail: invalid/key/escape.toml
- fail: invalid/key/newline-1.toml
- fail: invalid/key/newline-4.toml
- fail: invalid/key/newline-5.toml
- fail: invalid/spec/table-9-0.toml
- fail: invalid/spec/table-9-1.toml
- fail: invalid/string/literal-multiline-quotes-1.toml
- fail: invalid/string/literal-multiline-quotes-2.toml
- fail: invalid/string/multiline-quotes-1.toml
- fail: invalid/table/array-no-close-1.toml
- fail: invalid/table/array-no-close-2.toml
- fail: invalid/table/duplicate-key-dotted-table.toml
- fail: invalid/table/duplicate-key-dotted-table2.toml
- fail: invalid/table/llbrace.toml
- fail: invalid/table/redefine-2.toml
- fail: invalid/table/redefine-3.toml
- fail: invalid/table/rrbrace.toml
- fail: valid/datetime/no-seconds.toml
- fail: valid/key/like-date.toml
- fail: valid/key/numeric-02.toml
- fail: valid/key/numeric-05.toml
- fail: valid/spec/keys-7.toml
- fail: valid/string/escape-tricky.toml
- fail: valid/string/multiline-escaped-crlf.toml
- fail: valid/table/names.toml
- fail: valid/table/names-with-values.toml
passing: 508/557
```

## License
All microwave code is under the MIT license.
