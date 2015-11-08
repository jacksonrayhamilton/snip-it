;;; snip-it.el --- Expand snippets  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Jackson Ray Hamilton

;; Author: Jackson Ray Hamilton <jackson@jacksonrayhamilton.com>
;; Version: 0.0.0
;; Keywords: abbrev, convenience
;; Package-Requires: ((emacs "24.5"))
;; URL: https://github.com/jacksonrayhamilton/snip-it

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

;; Expand snippets.

;; Bind `snip-it-expand' to a key of your choice:
;;
;;  (global-set-key (kbd "<C-tab>") #'snip-it-expand)
;;
;; Save snippets to a directory with the name of a major mode.  Snippets' names
;; are determined by their filenames.  For instance, an "add" snippet for Emacs
;; Lisp would be saved to "~/.emacs.d/snippets/emacs-lisp-mode/add" and it would
;; look like this:
;;
;;  (setq $1 (+ $1 $2))
;;
;; Add the major mode's directory's parent to `snip-it-directories':
;;
;;  (add-to-list 'snip-it-directories "~/.emacs.d/snippets")
;;
;; In a lisp buffer, type "add" and invoke `snip-it-expand'.  Start typing.  Use
;; the same key bound to `snip-it-expand' to advance to the N+1th fields.

;;; Code:

(require 'thingatpt)

(defcustom snip-it-directories '()
  "Directories to search for snippets."
  :group 'snip-it)

(defun snip-it-exact-regexp (word)
  "Create a regexp matching exactly WORD."
  (concat "\\`" (regexp-quote word) "\\'"))

(defun snip-it-find-in-directory (directory match)
  "Return file from DIRECTORY matching regexp MATCH."
  (let ((files (directory-files directory nil match)))
    (when files (car files))))

(defun snip-it-read-file (path)
  "Read contents from file at PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun snip-it-find-snippet (name mode)
  "Find snippet named NAME for MODE."
  (let ((mode-directory
         (catch 'directory
           (mapc
            (lambda (directory)
              (let ((mode-directory (format "%s/%s" directory mode)))
                (when (file-directory-p mode-directory)
                  (throw 'directory mode-directory))))
            snip-it-directories)
           ;; Return nil if the search fails.
           nil))
        (name-regexp (snip-it-exact-regexp name)))
    (when (and mode-directory
               (snip-it-find-in-directory mode-directory name-regexp))
      (snip-it-read-file (format "%s/%s" mode-directory name)))))

(defun snip-it-interpolate (pieces values)
  "Replace variables in PIECES with VALUES."
  (let ((string "") piece)
    (while pieces
      (setq piece (car pieces))
      (setq pieces (cdr pieces))
      (setq string (concat
                    string
                    (cond
                     ((eq piece 0) "")
                     ((numberp piece)
                      (nth (1- piece) values))
                     (piece)))))
    string))

(defun snip-it-slice (seq start &optional end)
  "Return copy of SEQ from START to END."
  (cond ((eq start end) '())
        (t
         (let ((slice (copy-sequence (nthcdr start seq))))
           (when end
             (setcdr (nthcdr (- end start 1) slice) nil))
           slice))))

(defun snip-it-make-template-function (pieces)
  "Create function to concatentate PIECES around an index."
  (lambda (var-index var-values)
    (let ((search-pieces pieces)
          (search-index 0)
          found-index)
      (while search-pieces
        (cond
         ((eq (car search-pieces) var-index)
          (setq found-index search-index)
          (setq search-pieces nil))
         (t
          (setq search-pieces (cdr search-pieces))))
        (setq search-index (1+ search-index)))
      (list
       (snip-it-interpolate
        (snip-it-slice pieces 0 found-index)
        var-values)
       (snip-it-interpolate
        (snip-it-slice pieces (1+ found-index))
        var-values)))))

(defun snip-it-make-template (string)
  "Create expandable template from snippet STRING."
  (let* ((pieces '())
         (index 0)
         (start 0)
         (exit-p nil)
         char-string char
         var (varlist '())
         parse-var
         append-remaining)
    (setq parse-var
          (lambda ()
            (let ((continue t) (var ""))
              (while (and continue (< index (length string)))
                (setq char-string (substring string index (1+ index)))
                (setq char (string-to-char char-string))
                (cond
                 ((and (>= char 48) (<= char 57))
                  (setq var (concat var char-string))
                  (setq index (1+ index)))
                 (t
                  (setq continue nil))))
              (when (< (length var) 1)
                (error "Expected var number after \"$\" (e.g. \"$1\")"))
              (string-to-number var))))
    (setq append-remaining
          (lambda ()
            (let ((remaining (substring string start (min index (length string)))))
              (setq pieces (append pieces (list remaining))))))
    (while (< index (length string))
      (setq char-string (substring string index (1+ index)))
      (setq char (string-to-char char-string))
      (cond
       ((= char 36) ; $
        (funcall append-remaining)
        (setq index (1+ index))
        (setq var (funcall parse-var))
        (cond
         ((eq var 0)
          (setq exit-p t))
         ((not (member var varlist))
          (setq varlist (append varlist (list var)))))
        (setq pieces (append pieces (list var)))
        (setq start index))
       ((= char 92) ; \
        (setq index (1+ index))))
      (setq index (1+ index)))
    (funcall append-remaining)
    `((exit-p . ,exit-p)
      (arity . ,(length varlist))
      (function . ,(snip-it-make-template-function pieces)))))

(defun snip-it-constant (value)
  "Return a function that always returns VALUE."
  (lambda (&rest _unused) value))

(defun snip-it-times (n fn)
  "For N times, call FN, accumulating the results."
  (let ((index 0) (results '()))
    (while (< index n)
      (setq results (append results (list (funcall fn index))))
      (setq index (1+ index)))
    results))

(defvar-local snip-it-expand-next-function nil
  "Per-expansion progression handler.")

(defun snip-it-expand-next ()
  "Expand the next field in the current snippet."
  (interactive)
  (when snip-it-expand-next-function
    (funcall snip-it-expand-next-function)))

(defvar-local snip-it-expand-exit-function nil
  "Per-expansion abortion handler.")

(defun snip-it-expand-quit ()
  "Stop expanding the current snippet."
  (interactive)
  (when snip-it-expand-exit-function
    (funcall snip-it-expand-exit-function))
  (signal 'quit nil))

(defvar snip-it-expand-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap snip-it-expand] #'snip-it-expand-next)
    (define-key map [remap keyboard-quit] #'snip-it-expand-quit)
    map)
  "Keybindings for snippet expansion.")

(defun snip-it-expand-template (template)
  "Expand TEMPLATE."
  (let* ((exit-p (cdr (assq 'exit-p template)))
         (arity (cdr (assq 'arity template)))
         (function (cdr (assq 'function template)))
         (var-values (snip-it-times arity (snip-it-constant "")))
         (start-pos (point))
         (index -1)
         pieces
         value-length
         set-pieces
         next
         surround
         update
         exit)
    (setq set-pieces
          (lambda ()
            (setq pieces (funcall function (1+ index) var-values))))
    (setq next
          (lambda ()
            (setq index (1+ index))
            (cond
             ((< index arity)
              (setq value-length 0)
              (funcall set-pieces)
              (when (= index 0)
                (funcall surround))
              (goto-char (+ start-pos (length (nth 0 pieces)))))
             (t
              (when exit-p
                (setq pieces (funcall function 0 var-values))
                (goto-char (+ start-pos (length (nth 0 pieces)))))
              (funcall exit)))))
    (setq surround
          (lambda ()
            (let* ((value (nth index var-values)))
              (funcall set-pieces)
              (insert (nth 0 pieces))
              (forward-char (length value))
              (insert (nth 1 pieces)))))
    (setq update
          (lambda (start end length)
            (setq value-length (- (+ value-length (- end start)) length))
            (cond
             ((< value-length 0)
              ;; Invalid state; gracefully give up.
              (funcall exit))
             (t
              (let* ((cell (nthcdr index var-values))
                     (value-pos (+ start-pos (length (nth 0 pieces))))
                     (value-end (+ value-pos value-length))
                     (pieces-end (+ value-end (length (nth 1 pieces))))
                     (value (buffer-substring value-pos value-end)))
                (setcar cell value)
                (delete-region value-end pieces-end)
                (delete-region start-pos value-pos)
                (save-excursion
                  (goto-char start-pos)
                  (funcall surround))
                ;; Zero-length strings aren't treated like "pivots."
                (when (= (length value) 0)
                  (goto-char (+ start-pos (length (nth 0 pieces))))))))))
    (setq exit
          (lambda ()
            (setq overriding-terminal-local-map nil)
            (setq snip-it-expand-next-function nil)
            (setq snip-it-expand-exit-function nil)
            (remove-hook 'after-change-functions update)))
    (setq overriding-terminal-local-map snip-it-expand-map)
    (setq snip-it-expand-next-function next)
    (setq snip-it-expand-exit-function exit)
    (funcall next)
    (add-to-list 'after-change-functions update)))

;;;###autoload
(defun snip-it-expand ()
  "Expand snippet at point."
  (interactive)
  (let (word snippet template)
    (when (and (setq word (word-at-point))
               (setq snippet (snip-it-find-snippet word major-mode))
               (setq template (snip-it-make-template snippet)))
      (delete-char (* -1 (length word)))
      (snip-it-expand-template template))))

(provide 'snip-it)

;;; snip-it.el ends here
