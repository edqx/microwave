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
