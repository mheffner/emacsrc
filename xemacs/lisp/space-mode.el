;;;
;;; Minor Mode to Highlight Ill-Placed Whitespace, Using Fontification.
;;; Works with GNU Emacs 20 or higher.
;;;
;;; Copyright (C) 1998 Leif Kornstaedt <kornstae@ps.uni-sb.de>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;

;;
;; Put the following in your .emacs to activate:
;;
;;   (require 'space-mode)
;;   (space-mode 1)
;;

(make-face 'space-face)
(set-face-background 'space-face "hotpink")
(defvar space-face 'space-face
  "Face to use for highlighting ill-placed whitespace.")

(defvar space-mode t
  "*If non-nil, highlight ill-placed spaces.")

(make-variable-buffer-local 'space-mode)

(defun space-function ()
  (setq c-font-lock-keywords (list
			  '(("[ \t]+$"
			     (0 (cond (space-mode space-face))))
			    ("\\( +\\)\t"
			     (1 (cond (space-mode space-face))))
			    ("[^\t\n ].*\\(\t+\\)"
			     (1 (cond (space-mode space-face))))
			    ("^\\(        \\)+"
			     (0 (cond (space-mode space-face))))
			    ("\t\\(\\(        \\)+\\)"
			     (1 (cond (space-mode space-face))))))))

(defun space-mode (&optional arg)
  (interactive "P")
  "Toggle mode to highlight ill-placed whitespace.
With ARG, turn on space-mode if and only if arg is positive."
  (if (boundp 'font-lock-mode-hook)
      (let* ((current-state (member 'space-function font-lock-mode-hook))
	     (new-state (if arg
			    (> (prefix-numeric-value arg) 0)
			  current-state)))
	(if (and (not current-state) new-state)   ; turn it on
	    (add-hook 'font-lock-mode-hook 'space-function))
	(if (and current-state (not new-state))   ; turn it off
	    (remove-hook 'font-lock-mode-hook 'space-function)))))

(defun space-remove ()
  "Remove all ill-placed whitespace from the current buffer.
This is all the whitespace that is highlighted in space-mode."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((current-line (count-lines 1 (point))))
      (while (< (point) (point-max))
	(message "Removing ill-placed whitespace from line %s ..."
		 current-line)
	(if (looking-at "\t* ? ? ? ? ? ? ?\\($\\|[^ \t]\\)")
	    (goto-char (match-end 0))
	  (skip-chars-forward " \t")
	  (let ((col (current-column)))
	    (delete-horizontal-space)
	    (indent-to col)))
	(while (progn (skip-chars-forward "^\t\n")
		      (looking-at "\t"))
	  (let ((col1 (save-excursion
			(goto-char (match-beginning 0))
			(current-column)))
		(col2 (save-excursion
			(goto-char (match-end 0))
			(current-column))))
	    (replace-match "" nil t)
	    (insert-char ?  (- col2 col1))))
	(end-of-line)
	(delete-horizontal-space)
	(forward-line)
	(setq current-line (1+ current-line)))
      (message nil))))

(provide 'space-mode)

;; end of space-mode.el
