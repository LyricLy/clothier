# clothier

A reasonably compliant implementation of [Tailor](https://github.com/wompking/tailorlang).

## Requirements

- PCRE2 built with 8-bit support
- GNU readline

## Installation

```
git clone https://github.com/LyricLy/clothier
cd clothier
make
sudo make install
```
Remove with `sudo make uninstall`.

## Compatibility

`clothier` is mostly compatible with the reference implementation `tailor.py`, with the following exceptions:
- Colours are not supported yet. This will be fixed soon.
- `alter` does not yet support the `-a` and `-p` flags, and uses `$n` to reference capturing groups instead of `\1`. These issues will be fixed soon.
- `clothier` uses a different regex engine than `tailor.py`, and thus differs in a few places. For example, named capture groups use the syntax `(?<name>)` instead of `(?P<name>)`. This probably won't ever be fixed.
- `notch` and `see` are not supported yet. This will be fixed eventually.
