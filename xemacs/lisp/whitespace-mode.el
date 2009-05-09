;;; whitespace-mode.el -- minor mode for making whitespace visible

;; Copyright (C) 1994, 1995, 1996 Heiko Muenkel

;; Author: Heiko Muenkel <muenkel@tnt.uni-hannover.de>
;; Keywords: modes, extensions

;; This file is part of XEmacs.

;;  XEmacs is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2, or (at your
;;  option) any later version.

;;  XEmacs is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with XEmacs; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: FSF 19.34.
 
;;; Commentary:

;; $Id: whitespace-mode.el,v 1.18 1996/08/17 15:40:42 muenkel Exp $
;; Description:
;;
;;	This is a minor mode, which highlights whitespaces (blanks and
;;	tabs) with different faces, so that it is easier to
;;	distinguish between them.  
;;	Toggle the mode with: M-x whitespace-mode 
;;     or with: M-x whitespace-incremental-mode
;;	The second one should be used in big files.
;;
;;	If you want to know how the whitespaces are highlighted then
;;	type: M-x whitespace-show-faces
;;
;;	There are 2 hook variables `whitespace-incremental-mode-hook'
;;	and `whitespace-mode-hook' to customize the mode.
;;
;;	Look at the variable `whitespace-chars', if you only want to
;;	highlight tabs or blanks and not both.
;;
;;	Set `whitespace-install-toolbar-icon' to t, if you want a
;;	toolbar icon for this mode.
;;
;;	Set `whitespace-install-submenu' to t, if you want a submenu
;;     for this mode. Sorry, at the moment there is no menu for the
;;	Emacs 19. 
;;
;;	Thanks to Mike Scheidler for the toolbar icon code.
;; 
;; Installation:
;;   
;;     Put the files whitespace-mode.el and adapt.el in one of your
;; 	load-path directories and the following lines (without the
;; 	comment signs) in your .emacs (adapt.el is already in the
;;	XEmacs 19.12).
;;
;;     (autoload 'whitespace-mode "whitespace-mode" 
;;       "Toggle whitespace mode.
;;	With arg, turn whitespace mode on iff arg is positive.
;;	In whitespace mode the different whitespaces (tab, blank return)
;;	are highlighted with different faces. The faces are:
;;	`whitespace-blank-face', `whitespace-tab-face' and 
;;	`whitespace-return-face'."
;;	t)
;;
;;     (autoload 'whitespace-incremental-mode "whitespace-mode" 
;;	  "Toggle whitespace incremental mode.
;;     With arg, turn whitespace incremental mode on iff arg is positive.
;;	In whitespace incremental mode the different whitespaces (tab and 
;;	blank) are highlighted with different faces. The faces are:
;;	`whitespace-blank-face' and `whitespace-tab-face'.
;;	Use the command `whitespace-show-faces' to show their values.
;;	In this mode only these tabs and blanks are highlighted, which are in 
;;	the region from (point) - (window-heigh) to (point) + (window-heigh)."

;;; Code:

(provide 'whitespace-mode)
(require 'adapt)

;;; variables:

(defvar whitespace-chars 'tabs-and-blanks
  "*Determines, which whitespaces are highlighted.
Valid values are:
'tabs-and-blanks => tabs and blanks are highlighted;
'tabs            => only tabs are highlighted;
'blanks          => only blanks are highlighted;.

Changing this variable during the whitespace-*-mode is active could lead
to wrong highlighted whitespaces.")

(make-variable-buffer-local 'whitespace-chars)

(defvar whitespace-mode-hook nil
  "*Run after the `whitespace-mode' is switched on.")

(defvar whitespace-incremental-mode-hook nil
  "*Run after the `whitespace-incremental-mode' is switched on.")


(if (adapt-xemacsp)
(progn

(defvar whitespace-install-toolbar-icon nil
  "Set it to t, if a toolbar icon should be installed during loading this file.
The icon calls the function 'whitespace-toolbar-function'.")

(defvar whitespace-install-submenu nil
  "Set it to t, if a submenu should be installed during loading this file.")

))


(defvar whitespace-toolbar-function 'whitespace-incremental-mode
  "*The toolbar icon for the whitespace mode calls this function.
Valid values are: 'whitespace--mode and 'whitespace-incremental-mode.")

(defvar whitespace-blank-and-tab-search-string "\\( \\)\\|\\(\t\\)"
  "The regexp used to search for tabs and blanks.")

(defvar whitespace-tab-search-string "\t"
  "The search string used to find tabs.")

(defvar whitespace-blank-search-string " "
  "The search string used to find blanks.")

;;; Defining faces
(if (facep 'whitespace-blank-face)
    nil
  (make-face 'whitespace-blank-face)
  (set-face-background 'whitespace-blank-face "LightBlue1"))

(if (facep 'whitespace-tab-face)
    nil
  (make-face 'whitespace-tab-face)
  (set-face-background 'whitespace-tab-face "yellow")
  (set-face-underline-p 'whitespace-tab-face t))

(defun whitespace-show-faces ()
  "Shows the faces used by the `whitespace-mode'."
  (interactive)
  (save-excursion
    (let ((actual-buffer-name (buffer-name (current-buffer)))
	  (actual-whitespace-chars whitespace-chars)
	  (whitespace-mode-active (or whitespace-mode 
				      whitespace-incremental-mode))
	  (buffer (get-buffer-create "*Help*")))
      (set-buffer buffer)
      (setq whitespace-chars actual-whitespace-chars)
      (delete-region (point-min) (point-max))
      (insert "In the whitespace minor mode\n"
	      " this \" ")
      (whitespace-highlight-region (1- (point)) (point))
      (insert "\" is a blank, highlighted with `whitespace-blank-face' and\n"
	      " this \"\t")
      (whitespace-highlight-region (1- (point)) (point))
      (insert "\" is a tab,  highlighted with `whitespace-tab-face'.")
      
      (newline 2)
      (if (eq whitespace-chars 'blanks)
	  (insert 
	   "The highlighting of tabs is switched off.\n")
	(if (eq whitespace-chars 'tabs)
	    (insert
	     "The highlighting of blanks is switched off.\n")))
      (newline)
      (if whitespace-mode-active
	  (insert "A whitespace minor mode is active in the buffer\n  "
		  actual-buffer-name
		  ".\n")
	(insert "No whitespace minor mode is active in the buffer\n  "
		actual-buffer-name
		".\n"))
      (show-temp-buffer-in-current-frame buffer)
      )))

;;;
(defun whitespace-highlight-chars-in-region (char-string from to face)
  "Highlights the CHAR-STRING in the region from FROM to TO with the FACE."
  (while (search-forward char-string end t)
    (let ((extent))
      (cond ((match-beginning 0)
	     (setq extent (make-extent (match-beginning 0) (match-end 0)))
	     (set-extent-face extent face)
	     ))
      (set-extent-property extent 'start-open t)
      (set-extent-property extent 'end-open t)
      )))

(defun whitespace-highlight-region (from to)
  "Highlights the whitespaces in the region from FROM to TO."
  (let ((start (min from to))
	(end (max from to)))
    (save-excursion
      ;;    (message "Highlighting tabs and blanks...")
      (goto-char start)
      (cond ((eq whitespace-chars 'tabs-and-blanks)
	     (while (search-forward-regexp 
		     whitespace-blank-and-tab-search-string end t)
	       (let ((extent))
		 (cond ((match-beginning 1) ; blanks ?
			(setq extent (make-extent (match-beginning 1) 
						  (match-end 1)))
			(set-extent-face extent 'whitespace-blank-face)
			)
		       ((match-beginning 2) ; tabs ?
			(setq extent (make-extent (match-beginning 2) 
						  (match-end 2)))
			(set-extent-face extent 'whitespace-tab-face)
			)
		       )
		 (set-extent-property extent 'start-open t)
		 (set-extent-property extent 'end-open t)
		 )))
	    ((eq whitespace-chars 'tabs)
	     (whitespace-highlight-chars-in-region whitespace-tab-search-string 
						   from 
						   to
						   'whitespace-tab-face))
	    ((eq whitespace-chars 'blanks)
	     (whitespace-highlight-chars-in-region 
	      whitespace-blank-search-string 
	      from 
	      to
	      'whitespace-blank-face))
	    (t (error "ERROR: Bad value of whitespace-highlight-char")))
      ;;    (message "")
      )))

(defun whitespace-highlight-buffer ()
  "Highlights the whitespaces in the current buffer."
  (whitespace-highlight-region (point-min) (point-max))
)

(defsubst whitespace-find-next-highlighted-region (from to)
  "Returns nil or the next highlighted region."
  (map-extents '(lambda (extent dummy)
		 (if (extent-property extent 'whitespace-highlighted-region)
		     extent))
	       nil
	       from
	       to))

(defun whitespace-incremental-highlight (from to)
  "Highligthts the region from FROM to TO incremental."
  (save-excursion
    (goto-char from)
    (let ((extent (extent-at (point) nil 'whitespace-highlighted-region))
	  (next-extent nil)
	  (start nil))
      (while (< (point) to)
	(if extent
	    (goto-char (extent-end-position extent)))
	(if (< (point) to)
	    (progn
	      (setq start (point))
	      
	      (setq next-extent (whitespace-find-next-highlighted-region 
				 start
				 to))
	      (if extent
		  (if next-extent
		      (progn
			(set-extent-endpoints extent 
					      (extent-start-position extent)
					      (extent-end-position next-extent)
					      )
			(whitespace-highlight-region start
						     (1-
						      (extent-start-position
						       next-extent)))
			(delete-extent next-extent))
		    (set-extent-endpoints extent
					  (extent-start-position extent)
					  to)
		    (whitespace-highlight-region start to))
		(if next-extent
		    (progn
		      (setq extent next-extent)
		      (whitespace-highlight-region start 
						   (1- (extent-start-position
							next-extent)))
		      (set-extent-endpoints extent
					    start
					    (extent-end-position next-extent)))
		  (setq extent (make-extent start to))
		  (set-extent-property extent 'whitespace-highlighted-region t)
		  (whitespace-highlight-region start to)))
	      ))))))


(defun whitespace-highlight-window ()
  "Highlights the whitespaces in the current window."
  (whitespace-incremental-highlight (save-excursion
				      (forward-line (- (window-height)))
				      (point))
				    (save-excursion
				      (forward-line (window-height))
				      (point))))

(defun whitespace-dehighlight-region (start end)
  "Dehighlights the whitespaces in the region from START to END."
  (map-extents '(lambda (extent dummy)
		  (if (or (eq (extent-face extent) 'whitespace-blank-face)
			  (eq (extent-face extent) 'whitespace-tab-face)
			  (extent-property extent 
					   'whitespace-highlighted-region))
		      (progn
			(delete-extent extent)
			nil)))
	       nil
	       start
	       end
	       )
  )

(defun whitespace-dehighlight-buffer ()
  "Dehighlights the whitespaces in the current buffer."
  (whitespace-dehighlight-region (point-min) (point-max))
  )

(defun whitespace-highlight-after-change-function (beg end old-len)
  "Called, when any modification is made to buffer text.  Highlights
the whitespaces (blanks and tabs) in the region from BEG to
END. OLD-LEN isn't used, but provided from the after-change hook."
  (if (or (eq beg end)
	  (null whitespace-mode))
      nil
    (whitespace-dehighlight-region beg end)
    (whitespace-highlight-region beg end)))

(defvar whitespace-mode nil
  "Non-nil, if the `whitespace-mode' is active.")

(make-variable-buffer-local 'whitespace-mode)

(defun whitespace-mode (&optional arg)
  "Toggle whitespace mode.
With arg, turn whitespace mode on iff arg is positive.
In whitespace mode the different whitespaces (tab and blank)
are highlighted with different faces. The faces are:
`whitespace-blank-face' and `whitespace-tab-face'.
Use the command `whitespace-show-faces' to show their values."
  (interactive "P")
  (setq whitespace-mode
	(if (null arg) (not whitespace-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (and whitespace-mode whitespace-incremental-mode)
      (progn
	(whitespace-incremental-highlight (point-min) (point-max))
	(setq whitespace-incremental-mode nil)
	(remove-hook 'post-command-hook 'whitespace-highlight-window)
	(run-hooks 'whitespace-mode-hook)
	)
    (setq whitespace-incremental-mode nil)
    (remove-hook 'post-command-hook 'whitespace-highlight-window)
    (redraw-modeline) ;(force-mode-line-update)
    (if whitespace-mode
	(progn
	  (whitespace-highlight-buffer)
	  (make-local-variable 'after-change-functions)
	  (add-hook 'after-change-functions 
		    'whitespace-highlight-after-change-function)
	  (run-hooks 'whitespace-mode-hook))
      (whitespace-dehighlight-buffer)
      (remove-hook 'after-change-functions 
		   'whitespace-highlight-after-change-function)
      (remove-hook 'post-command-hook 'whitespace-highlight-window)
      )))

(defvar whitespace-incremental-mode nil
  "Non-nil, if the `whitespace-incremental-mode' is active.")

(make-variable-buffer-local 'whitespace-incremental-mode)

(defun whitespace-incremental-mode (&optional arg)
  "Toggle whitespace incremental mode.
With arg, turn whitespace incremental mode on iff arg is positive.
In whitespace incremental mode the different whitespaces (tab and blank)
are highlighted with different faces. The faces are:
`whitespace-blank-face' and `whitespace-tab-face'.
Use the command `whitespace-show-faces' to show their values.
In this mode only these tabs and blanks are highlighted, which are in 
the region from (point) - (window-heigh) to (point) + (window-heigh)."
  (interactive "P")
  (setq whitespace-incremental-mode
	(if (null arg) (not whitespace-incremental-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (and whitespace-mode whitespace-incremental-mode)
	(set-extent-property (make-extent (point-min) (point-max))
			     'whitespace-highlighted-region
			     t))
  (setq whitespace-mode nil)
  (redraw-modeline) ;(force-mode-line-update)
  ;(set-buffer-modified-p (buffer-modified-p)) ;No-op, but updates mode line.
  (if whitespace-incremental-mode
      (progn
	(whitespace-highlight-window)
	(make-local-variable 'post-command-hook)
	(add-hook 'post-command-hook 'whitespace-highlight-window)
	(make-local-variable 'after-change-functions)
	(add-hook 'after-change-functions 
		  'whitespace-highlight-after-change-function)
	(run-hooks 'whitespace-incremental-mode-hook))
    (whitespace-dehighlight-buffer)
    (remove-hook 'after-change-functions 
		 'whitespace-highlight-after-change-function)
    (remove-hook 'post-command-hook 'whitespace-highlight-window)
    ))


;;; Add whitespace-mode and whitespace-incremental-mode to the minor-mode-alist

(or (assq 'whitespace-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(whitespace-mode " WSP") minor-mode-alist)))

(or (assq 'whitespace-incremental-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(whitespace-incremental-mode " WSPI") minor-mode-alist)))


;;; Menu for the whitespace mode

(defun whitespace-set-whitespace-chars (new-whitespace-chars)
  "Sets the variable `whitespace-chars' and activates the change."
  (interactive (list (read (completing-read "Whitespaces to highlight: "
					    '(("tabs-and-blanks")
					      ("tabs")
					      ("blanks"))
					    nil
					    t
					    (symbol-name 'whitespace-chars)))))
  (if (eq whitespace-chars new-whitespace-chars)
      nil ; nothing to do
    (setq whitespace-chars new-whitespace-chars)
    (setq-default whitespace-chars new-whitespace-chars)
    (cond (whitespace-mode (whitespace-mode) 
			   (whitespace-mode))
	  (whitespace-incremental-mode (whitespace-incremental-mode)
				       (whitespace-incremental-mode))
	  )))

(defvar whitespace-menu nil
  "A menu for the whitespace minor mode.")
  
(setq whitespace-menu
      '("Whitespace Menu"
	["Highlight Whitespaces" 
	 whitespace-mode 
	 :style toggle 
	 :selected whitespace-mode]
	["Incremental Highlighting"
	 whitespace-incremental-mode
	 :style toggle
	 :selected whitespace-incremental-mode
	 ]
	"---"
	["Show Whitespace Faces" whitespace-show-faces t]
	"---"
	["Highlight Tabs & Blanks" 
	 (whitespace-set-whitespace-chars 'tabs-and-blanks)
	 :style radio
	 :selected (eq whitespace-chars 'tabs-and-blanks)]
	["Highlight Only Tabs"
	 (whitespace-set-whitespace-chars 'tabs)
	 :style radio
	 :selected (eq whitespace-chars 'tabs)]
	["Highlight Only Blanks"
	 (whitespace-set-whitespace-chars 'blanks)
	 :style radio
	 :selected (eq whitespace-chars 'blanks)]
	))

(if (and (boundp 'whitespace-install-submenu) whitespace-install-submenu)
    (add-submenu '("Apps") whitespace-menu))

;;; Toolbar icon for the XEmacs

(if (featurep 'toolbar)

(defvar toolbar-wspace-icon
  (toolbar-make-button-list
   "/* XPM */
static char * whitespace[] = {
\"28 28 4 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c black\",
\"X	c Gray60\",
\"o	c white\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"         ..      .          \",
\"       XXX.XXXXXX   .       \",
\"       Xoo.oooooXX  .       \",
\" .. .. ..o.o..oo..X...  ..  \",
\"  .  . X.o..o.ooX. X.  .  . \",
\"  .  . .oo.oo.ooX.XX.  .... \",
\"   ... .oo.oo.ooo.oo.  .    \",
\"   .  .Xoo.oo.ooo.oo.  .  . \",
\"   .  .Xo...o..o...o..  ..  \",
\"       XooooooooooooX       \",
\"       XooooooooooooX       \",
\" .... ....ooo...ooo...  ..  \",
\" .  .  .oo.o.oo.oo.oX. .  . \",
\"  .    .oo.ooo..oo.oX  .... \",
\"   ..  .oo.o..o.oo.oX  .    \",
\" .  .  .oo.o.oo.oo.oX. .  . \",
\" ....  ...oo.....oo..   ..  \",
\"       .ooooooooooooX       \",
\"       .XXXXXXXXXXXXX       \",
\"       .                    \",
\"      ...                   \",
\"                            \",
\"                            \",
\"                            \"
};")
  "A whitespace icon.")
)

(defun whitespace-toolbar-function ()
  "Calls the function determined by `whitespace-toolbar-function'."
  (interactive)
  (call-interactively whitespace-toolbar-function))

(if (and (adapt-xemacsp)
	 whitespace-install-toolbar-icon
	 (featurep 'toolbar) 
	 (eq (device-type (selected-device)) 'x))
    (add-spec-list-to-specifier 
     default-toolbar
     '((global 
	(nil
	 [toolbar-file-icon     find-file       t       "Open a file"   ]
	 [toolbar-folder-icon   dired           t       "View directory"]
	 [toolbar-disk-icon     save-buffer     t       "Save buffer"   ]
	 [toolbar-printer-icon  print-buffer    t       "Print buffer"  ]
	 [toolbar-cut-icon      x-kill-primary-selection   t "Kill region"]
	 [toolbar-copy-icon     x-copy-primary-selection   t "Copy region"]
	 [toolbar-paste-icon    
	  x-yank-clipboard-selection t "Paste from clipboard"]
	 [toolbar-undo-icon     undo            t       "Undo edit"     ]
	 [toolbar-replace-icon  query-replace   t       "Replace text"  ]
	 [toolbar-wspace-icon  
	  whitespace-toolbar-function t "Toggle whitespace mode"]
	 nil
	 [toolbar-compile-icon  toolbar-compile t       "Compile"       ]
	 [toolbar-debug-icon    toolbar-debug   t       "Debug"         ]
	 [toolbar-spell-icon    toolbar-ispell  t       "Spellcheck"    ]
	 [toolbar-mail-icon     toolbar-mail    t       "Mail"          ]
	 [toolbar-news-icon     toolbar-news    t       "News"          ]
	 [toolbar-info-icon     toolbar-info    t       "Information"   ]
	 )))))

;;; whitespace-mode.el ends here
