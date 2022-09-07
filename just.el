;;; just.el --- The big collection of small functions

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/just.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The big collection of small functions for the Emacs.  Inspired by
;; `dash', `s' and `f'

;;; Code:

(require 'dash)
(require 's)

(defun just-reverse-alist (alist)
  "Swap keys and values of ALIST.
For example, from its:

```\((p . 3) (l . 2)\)```

Return its:

```\((3 . p) (2 . l)\)```"
  (--map (cons (cdr it) (car it)) alist))

(defun just-text-at-line (&optional pos is-trim)
  "Return the line as string at POS, POS defaults to `point'.

POS defaults to `point'.  When IS-TRIM is non-nil return trimmed text at line"
  (setq pos (or pos (point)))
  (-->
   (save-excursion
     (goto-char pos)
     (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
   (if is-trim (s-trim it) it)))

(defun just-forward-point-at-regexp (regexp &optional bound count)
  "Return the position of a next match with REGEXP.

Return nil, if REGEXPS don't matches with the buffer text.

The optional second argument BOUND is a buffer position that bounds
the search.  The match found must not end after that position.  A
value of nil means search to the end of the accessible portion of
the buffer.

The optional third argument COUNT is a number that indicates the
search direction and the number of occurrences to search for.  If it
is positive, search forward for COUNT successive occurrences; if it
is negative, search backward, instead of forward, for -COUNT
occurrences.  A value of nil means the same as 1.

With COUNT positive/negative, the match found is the COUNTth/-COUNTth
one in the buffer located entirely after/before the origin of the
search."
  (save-excursion
    (and                                ;nofmt
     (search-forward-regexp regexp bound t count)
     (point))))

(defun just-backward-point-at-regexp (regexp &optional bound count)
  "Return the position of a previous match with REGEXP.

Return nil, if REGEXPS don't matches with the buffer text.

The optional second argument BOUND is a buffer position that bounds
the search.  The match found must not end after that position.  A
value of nil means search to the end of the accessible portion of
the buffer.

The optional third argument COUNT is a number that indicates the
search direction and the number of occurrences to search for.  If it
is positive, search backward for COUNT successive occurrences; if it
is negative, search forward, instead of backward, for -COUNT
occurrences.  A value of nil means the same as 1.

With COUNT positive/negative, the match found is the COUNTth/-COUNTth
one in the buffer located entirely after/before the origin of the
search."
  (just-forward-point-at-regexp regexp bound (- (or count 1))))

(defun just-search-backward-one-of-regexp (regexps &optional bound count)
  "Go to the backward match with one of REGEXPS which is closest.

Point of each match with one of REGEXPS should be  via
`just-forward-point-at-regexp' with passed arguments BOUND.

Repeat it COUNT times, if COUNT isn't positive, then do forward search"
  (just-search-forward-one-of-regexp regexps bound (- (or count 1))))

(defun just-search-forward-one-of-regexp (regexps &optional bound count)
  "Go to the forward match with one of REGEXPS which is closest.

Point of each match with one of REGEXPS should be found via
`just-forward-point-at-regexp' with passed arguments BOUND.

Repeat it COUNT times, if COUNT isn't positive, then do bacward search"
  (or count (setq count 1))
  (let ((forward-point (point))
        (n (1+ (abs count))))
    (while (and forward-point (not (zerop n)))
      (goto-char forward-point)
      (setq forward-point
            (just--forward-closest-regexp-match-point regexps
                                                      bound
                                                      count))
      (decf n))))

(defun just--forward-closest-regexp-match-point (regexps &optional bound count)
  "Go to the match with one of REGEXPS which is closest with the current point.

Point of each match with one of REGEXPS should be found via
`just-forward-point-at-regexp' with passed arguments BOUND.

COUNT will be ignored as integer and will be readed as `signum' (0, -1, 1) for
the backward/forward search."
  (or count (setq count 1))
  (-some->> regexps
    (--map (just-forward-point-at-regexp it bound (signum count)))
    (-filter 'numberp)
    (apply 'min)))

(defun just--regexp-prefix (prefix s)
  "Return t when the regexp PREFIX match with S as prefix."
  (-some->>
      s
    (s-matched-positions-all prefix)
    (-first-item)
    (car)                                ; the beginning of the matched position
    (= 0)))

(defun just-line-regexp-prefix-p (p &optional pos)
  "Return t, when text of line at POS start with the regexp P.

POS defaults to `point'."
  (just--regexp-prefix p (just-text-at-line pos)))

(defun just-line-prefix-p (p &optional pos is-trim)
  "Return t, when text of line at POS start with P.

POS defaults to `point'.  When IS-TRIM is non-nil before check prefix trim text
at line"
  (s-prefix-p p (just-text-at-line pos is-trim)))

(defun just-line-suffix-p (p &optional pos is-trim)
  "Return t, when text of line at POS ends with P.

POS defaults to `point'.  When IS-TRIM is non-nil before check prefix trim text
at line"
  (s-suffix-p p (just-text-at-line pos is-trim)))

(defun just-line-is-whitespaces-p (&optional pos)
  "Return t, when line at POS hasn't text symbols."
  (setq pos (or pos (point)))
  (->> pos (just-text-at-line) (s-trim) (string-equal "")))

(defun just-line-has-text-p (&optional pos)
  "Return t, when line at POS has text symbols."
  (not (just-line-is-whitespaces-p pos)))

(defun just-buffers-with-ext (ext)
  "Get list of opened buffers, which extension EXT."
  (->>
   (buffer-list)
   (--filter (f-ext-p (buffer-name it) ext))))

(defun just-completing-read-numbers (prompt ; nofmt
                                     collection
                                     &optional
                                       predicate
                                       require-match
                                       initial-input
                                       hist
                                       def
                                       inherit-input-method)
  "Read number from the minibuffer, with completion to one of COLLECTION.
For documentation of PROMPT PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD, see to original function `completing-read'"
  (-->
   collection
   (-sort #'< it)
   (-map #'number-to-string it)
   (completing-read
    prompt
    it
    predicate
    require-match
    initial-input
    hist
    def
    inherit-input-method)
   (string-to-number it)))

(defmacro just-for-each-line-when* (begin end pred &rest body)
  "Eval BODY on start of each line from BEGIN to END when PRED eval to non-nil."
  (declare (indent 3))
  `(just-for-each-line* ,begin ,end (when ,pred ,@body)))

(defun just-for-each-line-when (begin end pred f)
  "Run F on start of each line between BEGIN and END when PRED get non-nil."
  (just-for-each-line-when* begin end (funcall pred) (funcall f)))

(defmacro just-for-each-line* (begin end &rest body)
  "Evaluate BODY on start of each line from point BEGIN to END."
  (declare (indent 2))
  `(save-excursion
     (goto-char ,begin)
     (beginning-of-line)
     (while (< (point) ,end)
       ,@body
       (forward-line))))

(defun just-for-each-line (begin end f)
  "Run F on start of each line beetween BEGIN and END."
  (just-for-each-line* begin end (funcall f)))

(defmacro just-call-on-next-line* (&rest body)
  "Evaluate BODY on next line, saving excursion."
  `(save-excursion (forward-line) ,@body))

(defun just-call-on-next-line (f)
  "Run F on next line, saving excursion."
  (just-call-on-next-line* (funcall f)))

(defmacro just-call-on-prev-line* (&rest body)
  "Evaluate BODY on previous line, saving excursion."
  `(save-excursion (forward-line -1) ,@body))

(defun just-call-on-prev-line (f)
  "Run F on previous line, saving excursion."
  (just-call-on-prev-line* (funcall f)))

(defun just-mark-region (beg end)
  "Mark region between BEG and END."
  (push-mark beg nil t)
  (goto-char end))

(defun just-text-in-region ()
  "If the region is active, return text in the region, otherwise return nil."
  (when (region-active-p)
    (buffer-substring (region-beginning) (region-end))))

(defun just-ensure-empty-line (&optional pos)
  "If the line at POS isn't empty, then make new empty line after it.

NOTE: no saving excursion"
  (interactive)
  (or pos (setq pos (point)))
  (unless (just-line-is-whitespaces-p pos) ;nofmt
    (goto-char pos)
    (end-of-line)
    (newline-and-indent)))

(provide 'just)
;;; just.el ends here
