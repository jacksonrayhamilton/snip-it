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

(require 'cl-lib)
(require 'thingatpt)

(defcustom snip-it-directories '()
  "Directories to search for snippets."
  :group 'snip-it)

(defun snip-it-read-file (path)
  "Read contents from file at PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun snip-it-find-snippet (name mode)
  "Find snippet named NAME for MODE."
  (let (snippet)
    (cl-some
     (lambda (directory)
       (let ((filename (format "%s/%s/%s" directory mode name)))
         (and (file-readable-p filename)
              (setq snippet filename))))
     snip-it-directories)
    (when snippet
      (snip-it-read-file snippet))))

(defun snip-it-interpolate (pieces values)
  "Replace variables in PIECES with VALUES."
  (cl-reduce
   (lambda (string piece)
     (concat string (cond
                     ((eq piece 0) "")
                     ((numberp piece)
                      (nth (1- piece) values))
                     (piece))))
   pieces
   :initial-value ""))

(defun snip-it-make-template-function (pieces)
  "Create function to concatentate PIECES around an index."
  (lambda (pivot-var var-values)
    (let ((pivot-index (cl-position pivot-var pieces)))
      (list
       (snip-it-interpolate
        (cl-subseq pieces 0 pivot-index)
        var-values)
       (snip-it-interpolate
        (cl-subseq pieces (1+ pivot-index))
        var-values)))))

(cl-defstruct snip-it-template
  exit-p
  arity
  function)

(defun snip-it-make-template (string)
  "Create expandable template from snippet STRING."
  (let* ((pieces '())
         (varlist '())
         (index 0)
         (start 0)
         (exit-p nil)
         char append-remaining parse-var)
    (setq append-remaining
          (lambda ()
            (let* ((end (min index (length string)))
                   (remaining (substring string start end)))
              (setq pieces (append pieces (list remaining))))))
    (setq parse-var
          (lambda ()
            (let (var)
              (funcall append-remaining)
              (cl-incf index)
              (cond
               ((string-match "[[:digit:]]+" string index)
                (setq var (match-string 0 string))
                (cl-incf index (length var))
                (setq var (string-to-number var)))
               (t
                (error "Expected var number after \"$\" (e.g. \"$1\")")))
              (cond
               ((eq var 0)
                (setq exit-p t))
               ((not (member var varlist))
                (setq varlist (append varlist (list var)))))
              (setq pieces (append pieces (list var)))
              (setq start index))))
    (while (< index (length string))
      (setq char (string-to-char (substring string index (1+ index))))
      (cond
       ((= char 36) ; $
        (funcall parse-var))
       ((= char 92) ; \
        ;; Skip an escape character.
        (cl-incf index)))
      (cl-incf index))
    (funcall append-remaining)
    (make-snip-it-template
     :exit-p exit-p
     :arity (length varlist)
     :function (snip-it-make-template-function pieces))))

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
  (let* ((exit-p (snip-it-template-exit-p template))
         (arity (snip-it-template-arity template))
         (function (snip-it-template-function template))
         (var-values (make-list arity ""))
         (start-pos (point))
         (index -1)
         pieces value-length
         set-pieces next surround update exit)
    (setq set-pieces
          (lambda ()
            (setq pieces (funcall function (1+ index) var-values))))
    (setq next
          (lambda ()
            (cl-incf index)
            (cond
             ((< index arity)
              (setq value-length 0)
              (cond
               ((= index 0)
                (funcall surround))
               (t
                (funcall set-pieces)))
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
