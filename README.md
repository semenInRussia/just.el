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


### just-reverse-alist `(alist)`

Swap keys and values of `ALIST`.
For example, from its:

```((p . 3) (l . 2))```

Return its:

```((3 . p) (2 . l))```

### just-text-at-line `(&optional pos is-trim)`

Return the line as string at `POS`, `POS` defaults to `point`.

`POS` defaults to `point`.  When `IS-TRIM` is non-nil return trimmed text at line

### just-forward-point-at-regexp `(regexp &optional bound count)`

Return the position of a next match with `REGEXP`.

Return nil, if REGEXPS don't matches with the buffer text.

The optional second argument `BOUND` is a buffer position that bounds
the search.  The match found must not end after that position.  A
value of nil means search to the end of the accessible portion of
the buffer.

The optional third argument `COUNT` is a number that indicates the
search direction and the number of occurrences to search for.  If it
is positive, search forward for `COUNT` successive occurrences
is negative, search backward, instead of forward, for -`COUNT`
occurrences.  A value of nil means the same as 1.

With `COUNT` positive/negative, the match found is the COUNTth/-COUNTth
one in the buffer located entirely after/before the origin of the
search.

### just-backward-point-at-regexp `(regexp &optional bound count)`

Return the position of a previous match with `REGEXP`.

Return nil, if REGEXPS don't matches with the buffer text.

The optional second argument `BOUND` is a buffer position that bounds
the search.  The match found must not end after that position.  A
value of nil means search to the end of the accessible portion of
the buffer.

The optional third argument `COUNT` is a number that indicates the
search direction and the number of occurrences to search for.  If it
is positive, search backward for `COUNT` successive occurrences
is negative, search forward, instead of backward, for -`COUNT`
occurrences.  A value of nil means the same as 1.

With `COUNT` positive/negative, the match found is the COUNTth/-COUNTth
one in the buffer located entirely after/before the origin of the
search.

### just-search-backward-one-of-regexp `(regexps &optional bound count)`

Go to the backward match with one of `REGEXPS` which is closest.

Point of each match with one of `REGEXPS` should be  via
`just-forward-point-at-regexp` with passed arguments `BOUND`.

Repeat it `COUNT` times, if `COUNT` isn`t positive, then do forward search

### just-search-forward-one-of-regexp `(regexps &optional bound count)`

Go to the forward match with one of `REGEXPS` which is closest.

Point of each match with one of `REGEXPS` should be found via
`just-forward-point-at-regexp` with passed arguments `BOUND`.

Repeat it `COUNT` times, if `COUNT` isn`t positive, then do bacward search

### just--forward-closest-regexp-match-point `(regexps &optional bound arrow)`

Go to the match with one of `REGEXPS` which is closest with the current point.

Point of each match with one of `REGEXPS` should be found via
`just-forward-point-at-regexp` with passed arguments `BOUND`.

`ARROW` refers to the way in which will do search, if `ARROW` is positive then
searches will be forward, if `ARROW` is negative, then searches will be backward.

### just--min-by `(compare nums)`

Return the minimal of the `NUMS` comaring with evaluating of the `COMPARE`.

### just--regexp-prefix `(prefix s)`

Return t when the regexp `PREFIX` match with `S` as `prefix`.

### just-line-regexp-prefix-p `(p &optional pos)`

Return t, when text of line at `POS` start with the regexp `P`.

`POS` defaults to `point`.

### just-line-prefix-p `(p &optional pos is-trim)`

Return t, when text of line at `POS` start with `P`.

`POS` defaults to `point`.  When `IS-TRIM` is non-nil before check prefix trim text
at line

### just-line-suffix-p `(p &optional pos is-trim)`

Return t, when text of line at `POS` ends with `P`.

`POS` defaults to `point`.  When `IS-TRIM` is non-nil before check prefix trim text
at line

### just-line-is-whitespaces-p `(&optional pos)`

Return t, when line at `POS` hasn`t text symbols.

### just-line-has-text-p `(&optional pos)`

Return t, when line at `POS` has text symbols.

### just-buffers-with-ext `(ext)`

Get list of opened buffers, which extension `EXT`.

### just-completing-read-numbers `(prompt collection &optional predicate require-match initial-input hist def inherit-input-method)`

Read number from the minibuffer, with completion to one of `COLLECTION`.
For documentation of `PROMPT` `PREDICATE` `REQUIRE-MATCH` `INITIAL-INPUT` `HIST` `DEF`
`INHERIT-INPUT-METHOD`, see to original function `completing-read`

### just-for-each-line-when* `(begin end pred &rest body)`

Eval `BODY` on start of each line from `BEGIN` to `END` when `PRED` eval to non-nil.

### just-for-each-line-when `(begin end pred f)`

Run `F` on start of each line between `BEGIN` and `END` when `PRED` get non-nil.

### just-for-each-line* `(begin end &rest body)`

Evaluate `BODY` on start of each line from point `BEGIN` to `END`.

### just-for-each-line `(begin end f)`

Run `F` on start of each line beetween `BEGIN` and `END`.

### just-call-on-next-line* `(&rest body)`

Evaluate `BODY` on next line, saving excursion.

### just-call-on-next-line `(f)`

Run `F` on next line, saving excursion.

### just-call-on-prev-line* `(&rest body)`

Evaluate `BODY` on previous line, saving excursion.

### just-call-on-prev-line `(f)`

Run `F` on previous line, saving excursion.

### just-mark-region `(beg end)`

Mark region between `BEG` and `END`.

### just-mark-region-between-movements `(start-move &optional end-move)`

Mark region beetwen 2 points of the call `START-MOVE` and `END-MOVE`.

### just-text-in-region `()`

If the region is active, return text in the region, otherwise return nil.

### just-ensure-empty-line `(&optional pos)`

If the line at `POS` isn`t empty, then make new empty line after it.

NOTE: no saving excursion

### just-spaces-to-1 `()`

Delete spaces backward and insert one space instead.

### just-delete-chars-backward `(string &optional lim)`

Delete chars backward, stopping after a char not in `STRING`, or at pos `LIM`.

Returns the distance traveled, either zero or negative.

### just-with-same-buffer `(&rest body)`

Evaluate `BODY` switch to the initial buffer when end evaluating.
### just-reverse-alist `(alist)`

Swap keys and values of `ALIST`.
For example, from its:

```((p . 3) (l . 2))```

Return its:

```((3 . p) (2 . l))```

### just-text-at-line `(&optional pos is-trim)`

Return the line as string at `POS`, `POS` defaults to `point`.

`POS` defaults to `point`.  When `IS-TRIM` is non-nil return trimmed text at line

### just-forward-point-at-regexp `(regexp &optional bound count)`

Return the position of a next match with `REGEXP`.

Return nil, if REGEXPS don't matches with the buffer text.

The optional second argument `BOUND` is a buffer position that bounds
the search.  The match found must not end after that position.  A
value of nil means search to the end of the accessible portion of
the buffer.

The optional third argument `COUNT` is a number that indicates the
search direction and the number of occurrences to search for.  If it
is positive, search forward for `COUNT` successive occurrences
is negative, search backward, instead of forward, for -`COUNT`
occurrences.  A value of nil means the same as 1.

With `COUNT` positive/negative, the match found is the COUNTth/-COUNTth
one in the buffer located entirely after/before the origin of the
search.

### just-backward-point-at-regexp `(regexp &optional bound count)`

Return the position of a previous match with `REGEXP`.

Return nil, if REGEXPS don't matches with the buffer text.

The optional second argument `BOUND` is a buffer position that bounds
the search.  The match found must not end after that position.  A
value of nil means search to the end of the accessible portion of
the buffer.

The optional third argument `COUNT` is a number that indicates the
search direction and the number of occurrences to search for.  If it
is positive, search backward for `COUNT` successive occurrences
is negative, search forward, instead of backward, for -`COUNT`
occurrences.  A value of nil means the same as 1.

With `COUNT` positive/negative, the match found is the COUNTth/-COUNTth
one in the buffer located entirely after/before the origin of the
search.

### just-search-backward-one-of-regexp `(regexps &optional bound count)`

Go to the backward match with one of `REGEXPS` which is closest.

Point of each match with one of `REGEXPS` should be  via
`just-forward-point-at-regexp` with passed arguments `BOUND`.

Repeat it `COUNT` times, if `COUNT` isn`t positive, then do forward search

### just-search-forward-one-of-regexp `(regexps &optional bound count)`

Go to the forward match with one of `REGEXPS` which is closest.

Point of each match with one of `REGEXPS` should be found via
`just-forward-point-at-regexp` with passed arguments `BOUND`.

Repeat it `COUNT` times, if `COUNT` isn`t positive, then do bacward search

### just--forward-closest-regexp-match-point `(regexps &optional bound arrow)`

Go to the match with one of `REGEXPS` which is closest with the current point.

Point of each match with one of `REGEXPS` should be found via
`just-forward-point-at-regexp` with passed arguments `BOUND`.

`ARROW` refers to the way in which will do search, if `ARROW` is positive then
searches will be forward, if `ARROW` is negative, then searches will be backward.

### just--min-by `(compare nums)`

Return the minimal of the `NUMS` comaring with evaluating of the `COMPARE`.

### just--regexp-prefix `(prefix s)`

Return t when the regexp `PREFIX` match with `S` as `prefix`.

### just-line-regexp-prefix-p `(p &optional pos)`

Return t, when text of line at `POS` start with the regexp `P`.

`POS` defaults to `point`.

### just-line-prefix-p `(p &optional pos is-trim)`

Return t, when text of line at `POS` start with `P`.

`POS` defaults to `point`.  When `IS-TRIM` is non-nil before check prefix trim text
at line

### just-line-suffix-p `(p &optional pos is-trim)`

Return t, when text of line at `POS` ends with `P`.

`POS` defaults to `point`.  When `IS-TRIM` is non-nil before check prefix trim text
at line

### just-line-is-whitespaces-p `(&optional pos)`

Return t, when line at `POS` hasn`t text symbols.

### just-line-has-text-p `(&optional pos)`

Return t, when line at `POS` has text symbols.

### just-buffers-with-ext `(ext)`

Get list of opened buffers, which extension `EXT`.

### just-completing-read-numbers `(prompt collection &optional predicate require-match initial-input hist def inherit-input-method)`

Read number from the minibuffer, with completion to one of `COLLECTION`.
For documentation of `PROMPT` `PREDICATE` `REQUIRE-MATCH` `INITIAL-INPUT` `HIST` `DEF`
`INHERIT-INPUT-METHOD`, see to original function `completing-read`

### just-for-each-line-when* `(begin end pred &rest body)`

Eval `BODY` on start of each line from `BEGIN` to `END` when `PRED` eval to non-nil.

### just-for-each-line-when `(begin end pred f)`

Run `F` on start of each line between `BEGIN` and `END` when `PRED` get non-nil.

### just-for-each-line* `(begin end &rest body)`

Evaluate `BODY` on start of each line from point `BEGIN` to `END`.

### just-for-each-line `(begin end f)`

Run `F` on start of each line beetween `BEGIN` and `END`.

### just-call-on-next-line* `(&rest body)`

Evaluate `BODY` on next line, saving excursion.

### just-call-on-next-line `(f)`

Run `F` on next line, saving excursion.

### just-call-on-prev-line* `(&rest body)`

Evaluate `BODY` on previous line, saving excursion.

### just-call-on-prev-line `(f)`

Run `F` on previous line, saving excursion.

### just-mark-region `(beg end)`

Mark region between `BEG` and `END`.

### just-mark-region-between-movements `(start-move &optional end-move)`

Mark region beetwen 2 points of the call `START-MOVE` and `END-MOVE`.

### just-text-in-region `()`

If the region is active, return text in the region, otherwise return nil.

### just-ensure-empty-line `(&optional pos)`

If the line at `POS` isn`t empty, then make new empty line after it.

NOTE: no saving excursion

### just-spaces-to-1 `()`

Delete spaces backward and insert one space instead.

### just-delete-chars-backward `(string &optional lim)`

Delete chars backward, stopping after a char not in `STRING`, or at pos `LIM`.

Returns the distance traveled, either zero or negative.

### just-with-same-buffer `(&rest body)`

Evaluate `BODY` and switch to the initial buffer when end evaluating.
## Contributing
Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2022 Semen Khramtsov.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
