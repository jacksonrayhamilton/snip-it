;;; snip-it-test.el --- Tests for snip-it  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Jackson Ray Hamilton

;; This program is free software; you can redistribute it and/or modify
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

;; Tests for snip-it.

;; Use with `make test'.

;;; Code:

(require 'snip-it)

(defconst snip-it-test-path
  (file-name-directory (or load-file-name buffer-file-name))
  "This file's directory.")

(defmacro snip-it-test-deftest (name &rest body)
  "Define a test with NAME, executing BODY."
  (declare (indent defun))
  `(ert-deftest ,(intern (format "snip-it-%s" name)) ()
     (let ((snip-it-directories (format "%s/snippets" snip-it-test-path)))
       (with-temp-buffer
         (progn
           ,@body)))))

(snip-it-test-deftest mirroring-and-multiple-values ()
  (insert "add")
  (snip-it-expand)
  (insert "a")
  (snip-it-expand-next)
  (insert "1")
  (snip-it-expand-next)
  (should (equal (buffer-string) "(setq a (+ a 1))")))

;;; snip-it-test.el ends here
