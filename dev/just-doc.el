;;; just-doc.el --- Make documentation for `just'

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

;; Make documentation for `just'

;;; Code:

(require 'dash)
(require 's)
(require 'f)

(defgroup just-doc nil "Make doc for `just'." :group 'tool)

(defcustom just-doc-function-commands-symbols
  '(defmacro defun) ;nofmt
  "List of symbols, if first of sexp one of that, then this is function."
  :type '(list symbol)
  :group 'just-doc)

(defcustom just-doc-project-path
  "~/projects/just"
  "Path to the root of the project `just'."
  :type '(list symbol)
  :group 'just-doc)

(defcustom just-doc-readme-path
  (f-join just-doc-project-path "README.md")
  "Path to the README.md file in which `just-doc' will inject documentation."
  :type 'string
  :group 'just-doc)

(defcustom just-doc-source-path
  (f-join just-doc-project-path "just.el")
  "Path to the just.el source code file from which `just-doc' will parse info."
  :type 'string
  :group 'just-doc)

(defun just-doc-sync ()
  "Sync source code of `just' and README.md of `just'."
  (interactive)
  (just-doc-inject-to-readme just-doc-readme-path
                             just-doc-source-path))

(defun just-doc-inject-to-readme (readme source)
  "Add to README.md at path README documentation of functions at path SOURCE."
  (with-current-buffer (find-file readme)
    (search-forward-regexp "^#*\\W*Usage\n" nil t)
    (newline)
    (push-mark nil nil t)
    (search-forward-regexp "^#*\\W*Contributing" nil t)
    (beginning-of-line)
    (delete-region (region-beginning) (region-end))
    (deactivate-mark)
    (insert (just-doc-from-file source))))

(defun just-doc-from-file (filename)
  "Make documentation from file at FILENAME, return markdown source."
  (just-doc-from-source (f-read filename)))

(defclass just-doc-function ()
  ((name :initarg :name :accessor just-doc-function-name)
   (docstring :initarg :docstring :accessor just-doc-function-docstring)
   (args :initarg :args :accessor just-doc-function-args))
  "Class for function of Emacs Lisp.")

(defun just-doc-from-source (source)
  "Make documentation from elisp SOURCE, return markdown source code."
  (->>
   source
   (just-doc-search-functions-in-source)
   (-map #'just-doc-function-to-markdown)
   (s-join "\n")))

(defun just-doc-search-functions-in-source (source)
  "Search defnitions of functions in Emacs Lisp SOURCE.

Return list of `just-doc-function' objects."
  (->>
   source
   (just-doc-remove-elisp-comments)
   (just-doc-read-all-sexps)
   (-keep #'just-doc-function-from-sexp)
   (-remove #'just-doc-ignored-function-p)))

(defun just-doc-read-all-sexps (source)
  "Read all Lisp sexps from SOURCE."
  (read (format "(%s)" source)))

(defun just-doc--as-separated-word (string)
  "Return regexp which match to STRING as word with whitespaces around."
  (format "\\<%s\\>" string))

(defun just-doc-remove-elisp-comments (source)
  "Remove from elisp SOURCE code comments with ;;."
  (->>
   source
   (s-lines)
   (--remove (s-prefix-p ";" (s-trim it)))
   (--map (-first-item (s-split ";" it)))
   (s-join "\n")))

(defun just-doc-function-from-sexp (sexp)
  "Make `just-doc-function' from SEXP.

SEXP is list of symbols, like on Elisp source."
  (and
   (listp sexp)
   (-contains-p just-doc-function-commands-symbols (car sexp))
   (just-doc-function
    :name (symbol-name (-second-item sexp))
    :args (-third-item sexp)
    :docstring (-fourth-item sexp))))

(defun just-doc-ignored-function-p (fun)
  "Return t, when an `just-doc-function' object FUN shouldn't be viewed."
  (s-starts-with-p "just--" (just-doc-function-name fun)))

(defun just-doc-function-to-markdown (fun)
  "Transform FUN (obj `just-doc-function') to markdown source code."
  (let ((name (just-doc-function-name fun))
        (docstring (just-doc-function-markdown-docstring fun))
        (args
         (just-doc-function-args-to-markdown
          (just-doc-function-args fun))))
    (s-lex-format "### ${name} `${args}`

${docstring}
")))

(defun just-doc-function-args-to-markdown (fun-args)
  "Transform FUN-ARGS to a markdown source."
  (if fun-args (format "%s" fun-args) "()"))

(defun just-doc-function-markdown-docstring (fun)
  "Convert elisp docstring of FUN to markdown source."
  (->>
   (--reduce-from
    (s-replace-regexp
     (just-doc--as-separated-word (symbol-name it))
     (just-doc--markdown-denote it)
     acc)
    (just-doc-function-docstring fun)
    (just-doc-function-args fun))
   (s-replace-regexp "`\\(.+?\\)'" "`\\1`")
   ;; split to paragraphs
   (s-split "\n\n")
   (--map (s-replace "\n" " " it))
   ;; join the paragraphs back
   (s-join "\n\n")))

(defun just-doc--markdown-denote (s)
  "Denote S with syntax of markdown.

`print` is example of the denoted word"
  (format "`%s`" s))

(provide 'just-doc)
;;; just-doc.el ends here
