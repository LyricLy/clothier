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
- I think there might be bugs I don't know about. I should look into that eventually.
- Colours are not supported yet. This will be fixed soon.
- `notch` and `see` are not supported yet. This will be fixed soon.
- `clothier` uses a different regex engine than `tailor.py`, and thus differs in a few places. For example, named capture groups use the syntax `(?<name>)` instead of `(?P<name>)`. This probably won't ever be fixed, though I may consider implementing a compatibility layer of some kind to help out a little bit.
