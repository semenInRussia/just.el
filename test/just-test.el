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

(ert-deftest just-check-line-is-whitespaces-p-at-empty-line
    ()
  (with-temp-buffer (should (just-line-is-whitespaces-p))))

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

(ert-deftest just-check-line-regexp-prefix-p
    ()
  (with-temp-buffer
    (insert "**** Important Text")
    (should (just-line-regexp-prefix-p "\\*+"))
    (newline)
    (insert "**** Important Text")
    (should (just-line-regexp-prefix-p "\\*+" (point-min)))))

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
    (set-mark 5)
    (should (string-equal (just-text-in-region) "text"))))

(ert-deftest just-check-ensure-empty-line
    ()
  (with-temp-buffer
    (insert "  Any text")
    (just-ensure-empty-line)
    (should (string-equal (buffer-string) "  Any text\n  "))
    (just-ensure-empty-line)
    (should (string-equal (buffer-string) "  Any text\n  "))))

(ert-deftest just-check-call-on-backward-char
    ()
  (with-temp-buffer
    (insert "12")
    (should (= (just-call-on-backward-char 'point) 2))
    (should (= (just-call-on-backward-char* (point)) 2))
    (should (= (point) 3))))

(ert-deftest just-check-forward-point-at-regexp
    ()
  (with-temp-buffer
    (insert
     "You're talking about Inception? We all wanna die, we're meeseeks! This
is because you give Morty Smith bad grades, bitch! You created a day
care for my dad?

got an extra pair right here. Save it for the Semantics Dome,
E.B. White. Thanks Noob Noob, this guy gets it.

Well, to be honest, I'm kind of grossed out with the sexual nature of
how everything unfolded. I Nothing'didn t know how sexual dragons. were I
kind to read into there! Don't even trip about your pants, dawg. We
of just wanted to do some D&D stuff, y'know?
Haha god-damn! We will return… in possibly different clothing. Aids!

Do you know how many characters there are in the Simpsons Morty?
There's like a-a billion characters, M-Morty. There was an episode
where Former President BUSH was their neighbor! Dont look at me! That
guy over there roped me into this. I dunno, some people would pay top
dollar for that kind of breakthrough. I'll be with Reuben in my
workshop while you guys are having another day in Phil Collin's
proverbial paradise.")
    (goto-char (point-min))
    (should (= (just-forward-point-at-regexp "^Do") 599))
    (should (= (just-forward-point-at-regexp "this" nil 2) 255))))

(ert-deftest just-check-search-forward-one-of-regexp
    ()
  (with-temp-buffer
    (insert "
\\begin{itemize}
\\item 1
\\item 2
\\item 3
\\item 4
\\end{itemize}

\\begin{itemize}
\\item 1
\\item 2
\\item 3
\\item 4
\\end{itemize}")
    (goto-char (point-min))
    (just-search-forward-one-of-regexp
     '("\\\\end{itemize}" "\\\\item"))
    (should (equal (just-text-at-line) "\\item 1"))
    (just-search-forward-one-of-regexp
     '("\\\\end{itemize}" "\\\\item")
     nil 4)
    (should (equal (just-text-at-line) "\\end{itemize}"))))

(ert-deftest just-check-delete-chars-backward
    ()
  (with-temp-buffer
    (insert "j.   ")
    (should (= (just-delete-chars-backward "., ") -4))
    (should (= (point) 2))))

(ert-deftest just-check-spaces-to-1
    ()
  (with-temp-buffer
    (insert "j.   ")
    (should (= (just-spaces-to-1) -2))
    (should (equal (just-text-at-line) "j. "))))

(ert-deftest just-check-mark-region-between-movements
    ()
  (with-temp-buffer
    (insert "    Napoli! Is champion!")
    (just-mark-region-between-movements
     'beginning-of-line-text
     'end-of-line)
    (should (= (region-beginning) 5))
    (should (= (region-end) 25))))

(ert-deftest just-check-mark-region-between-movements-end
    ()
  (with-temp-buffer
    (insert "    Napoli! Is champion!")
    (just-mark-region-between-movements
     'beginning-of-line-text
     'end-of-line)
    (should (= (region-beginning) 5))
    (should (= (region-end) 25))))

(ert-deftest just-check-with-same-buffer
    ()
  (with-temp-buffer
    (let ((messages-mode
           (just-with-same-buffer
             (set-buffer "*Messages*")
             major-mode)))
      (should (eq messages-mode 'messages-buffer-mode))
      (should-not (eq major-mode 'messages-buffer-mode)))))

(ert-deftest just-check-with-same-buffer-errors
    ()
  (with-temp-buffer
    (should-error
     (just-with-same-buffer
       (set-buffer "*Messages*")
       (error "Stranger bug of the language")))
    (should-not (eq major-mode 'messages-buffer-mode))))

(ert-deftest just-check-mark-region
    ()
  (with-temp-buffer
    (insert "Some very rangom text, with some errors< but this isn't matter.")
    (just-mark-region 5 9)
    (should (= (region-beginning) 5))
    (should (= (region-end) 9))))


(provide 'just-test)

;;; just-test.el ends here
