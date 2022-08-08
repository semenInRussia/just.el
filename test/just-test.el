;;; just-test.el --- Tests for just.el

;; Copyright (C) 2013 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>

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

;; Tests for just.el

;;; Code:

(require 'ert)
(require 'just)

(ert-deftest just-check-line-is-whitespaces-p
    ()
  (with-temp-buffer
    (insert "     ")
    (should (just-line-is-whitespaces-p))
    (insert "t")
    (should-not (just-line-is-whitespaces-p))))

(ert-deftest just-check-line-is-whitespaces-p-at-other-point
    ()
  (with-temp-buffer
    (insert "     ")
    (newline)
    (insert "a lot of text")
    (should (just-line-is-whitespaces-p (point-min)))))

(ert-deftest just-check-reverse-alist
    ()
  (should
   (equal
    '((a . 3)
      (b . 4))
    (just-reverse-alist '((3 . a) (4 . b))))))

(ert-deftest just-check-line-prefix-p
    ()
  (with-temp-buffer
    (insert "**** Important Text")
    (should (just-line-prefix-p "****"))
    (should-not (just-line-prefix-p "*****"))
    (newline)
    (should (just-line-prefix-p "****" (point-min)))
    (should-not (just-line-prefix-p "*****" (point-min)))))

(ert-deftest just-check-line-has-text-p
    ()
  (with-temp-buffer
    (insert "     ")
    (should-not (just-line-has-text-p))
    (insert "t")
    (should (just-line-has-text-p))))

(ert-deftest just-check-line-has-text-p-at-other-point
    ()
  (with-temp-buffer
    (insert "TEXT!")
    (newline)
    (should (just-line-has-text-p (point-min)))
    (newline)
    (should-not (just-line-has-text-p 8))))

(ert-deftest just-check-for-each-line
    ()
  (with-temp-buffer
    (insert "one!\ntwo!\nthree!")
    (just-for-each-line*
        (point-min)
        (point-max)
      (insert (number-to-string (line-number-at-pos))))
    (should (string-equal (buffer-string) "1one!\n2two!\n3three!"))))

(ert-deftest just-check-for-each-line
    ()
  (with-temp-buffer
    (insert "one!\ntwo\nthree!")
    (just-for-each-line-when*
        (point-min)
        (point-max)
        (just-line-suffix-p "!")
      (insert (number-to-string (line-number-at-pos))))
    (should (string-equal (buffer-string) "1one!\ntwo\n3three!"))))

(ert-deftest just-check-call-on-next-line
    ()
  (with-temp-buffer
    (insert "1")
    (newline)
    (insert "2")
    (goto-char (point-min))
    (just-call-on-next-line 'line-number-at-pos)
    (should (= (just-call-on-next-line 'line-number-at-pos) 2))
    (should (= (line-number-at-pos) 1))))

(ert-deftest just-check-call-on-next-line
    ()
  (with-temp-buffer
    (insert "1")
    (newline)
    (insert "2")
    (just-call-on-next-line 'line-number-at-pos)
    (should (= (just-call-on-prev-line 'line-number-at-pos) 1))
    (should (= (line-number-at-pos) 2))))

(ert-deftest just-check-text-in-region
    ()
  (with-temp-buffer
    (insert "Any text")
    (should-not (just-text-in-region))
    (push-mark 5 nil t)
    (should (string-equal (just-text-in-region) "text"))))

(provide 'just-test)

;;; just-test.el ends here
