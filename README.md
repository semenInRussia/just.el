# just

## Summary

The big collection of small functions for Emacs

## Installing

This cool library isn't exists on Melpa, so you must install this
library via `git`, use followed shell command for install the library

```shell
git clone https://github.com/semenInRussia/just.el.git
```

You need to add the library load path to `load-path`

```emacs-lisp
;; Here I cloned the library in `~/.emacs.d/' directory
(add-to-list 'load-path "~/.emacs.d/lisp/just")
```

In project you can copy only one file `just.el`, use followed shell
code:

```shell
wget https://raw.githubusercontent.com/semenInRussia/just.el/master/just.el
```

## Usage

### just--regexp-prefix `(prefix s)`

Return t when the regexp `PREFIX` matches with `S` as `prefix`.

### just-buffers-with-ext `(ext)`

Get list of opened buffers, which extension `EXT`.

### just-call-on-next-line `(f)`

Run `F` on next line, saving excursion.

### just-call-on-next-line* `(&rest body)`

Evaluate `BODY` on next line, saving excursion.

### just-call-on-prev-line `(f)`

Run `F` on previous line, saving excursion.

### just-call-on-prev-line* `(&rest body)`

Evaluate `BODY` on previous line, saving excursion.

### just-completing-read-numbers `(prompt collection &optional predicate require-match initial-input hist def inherit-input-method)`

Read number from the minibuffer, with completion to one of `COLLECTION`.
For documentation of `PROMPT` `PREDICATE` `REQUIRE-MATCH` `INITIAL-INPUT` `HIST` `DEF`
`INHERIT-INPUT-METHOD`, see to original function `completing-read`

### just-ensure-empty-line `(&optional pos)`

If the line at `POS` isn`t empty, then make new empty line after it.

NOTE: no saving excursion

### just-for-each-line `(begin end f)`

Run `F` on start of each line beetween `BEGIN` and `END`.

### just-for-each-line* `(begin end &rest body)`

Evaluate `BODY` on start of each line from point `BEGIN` to `END`.

### just-for-each-line-when `(begin end pred f)`

Run `F` on start of each line between `BEGIN` and `END` when `PRED` get non-nil.

### just-for-each-line-when* `(begin end pred &rest body)`

Eval `BODY` on start of each line from `BEGIN` to `END` when `PRED` eval to non-nil.

### just-line-has-text-p `(&optional pos)`

Return t, when line at `POS` has text symbols.

### just-line-is-whitespaces-p `(&optional pos)`

Return t, when line at `POS` hasn`t text symbols.

### just-line-prefix-p `(p &optional pos is-trim)`

Return t, when text of line at `POS` start with `P`.

`POS` defaults to `point`.  When `IS-TRIM` is non-nil before check prefix trim text
at line

### just-line-regexp-prefix-p `(p &optional pos)`

Return t, when text of line at `POS` start with the regexp `P`.

`POS` defaults to `point`.

### just-line-suffix-p `(p &optional pos is-trim)`

Return t, when text of line at `POS` ends with `P`.

`POS` defaults to `point`.  When `IS-TRIM` is non-nil before check prefix trim text
at line

### just-mark-region `(beg end)`

Mark region between `BEG` and `END`.

### just-reverse-alist `(alist)`

Swap keys and values of `ALIST`.
For example, from its:

```((p . 3) (l . 2))```

Return its:

```((3 . p) (2 . l))```

### just-text-at-line `(&optional pos is-trim)`

Return the line as string at `POS`, `POS` defaults to `point`.

`POS` defaults to `point`.  When `IS-TRIM` is non-nil return trimmed text at line

### just-text-in-region `nil`

If the region is active, return text in the region, otherwise return nil.
## Contributing
Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2022 Semen Khramtsov.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
