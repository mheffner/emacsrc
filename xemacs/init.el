
(add-to-list 'load-path "~/work/git_repo/xemacs/lisp")

;; key/mouse bindings

;; wheel mouse
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)

;; random binding
(define-key global-map [(f1)] 'delete-other-windows)
(define-key global-map [(f5)] 'buffer-menu)
(define-key global-map [(f6)] 'fume-list-functions)

;; xcscope bindings
(define-key global-map [(control f3)]  'cscope-set-initial-directory)
(define-key global-map [(control f4)]  'cscope-unset-initial-directory)
(define-key global-map [(control f5)]  'cscope-find-this-symbol)
(define-key global-map [(control f6)]  'cscope-find-global-definition)
(define-key global-map [(control f7)]
  'cscope-find-global-definition-no-prompting)
(define-key global-map [(control f8)]  'cscope-pop-mark)
(define-key global-map [(control f9)]  'cscope-next-symbol)
(define-key global-map [(control f10)] 'cscope-next-file)
(define-key global-map [(control f11)] 'cscope-prev-symbol)
(define-key global-map [(control f12)] 'cscope-prev-file)
(define-key global-map [(meta f9)]  'cscope-display-buffer)
(define-key global-map [(meta f10)] 'cscope-display-buffer-toggle)
(define-key global-map [(control h)] 'backward-delete-char)

;; for terminal
; (define-key global-map [(meta \[)] 'end-of-buffer)

;; some global variables
(setq-default c-tab-always-indent nil)
(setq-default delete-key-deletes-forward t)
(setq-default require-final-newline (quote ask))
(setq-default paren-mode (quote blink-paren))
(setq-default column-number-mode t)
(setq-default track-eol t)
(setq-default bar-cursor 2)
(setq-default make-backup-files nil)
(setq-default line-number-mode t)
(setq-default kill-whole-line t)
(setq-default display-time-day-and-date t)
(setq-default sgml-indent-data t)

;; html for php
;; (push ''("\\.php\\'" . html-mode) auto-mode-alist)

;; default major mode
(setq default-major-mode 'text-mode)

;; turn on auto-fill for textmode
(add-hook 'text-mode-hook
        '(lambda ()
                (auto-fill-mode 1)))

;; turn off fill-mode for html
(add-hook 'html-mode-hook
        '(lambda ()
                (auto-fill-mode 0)))

;;
;; Key bindings
;;
(global-set-key [(meta q)] 'fill-paragraph)

;; xcscope
(require 'xcscope)

;; function menu
(require 'func-menu)

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp")
;; (add-to-list 'load-path "/usr/lib/xemacs/xemacs-packages/lisp/psgml")

(require 'psgml)
(setq auto-mode-alist (cons '("\\.docbook$" . sgml-mode) auto-mode-alist))
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)

;; whitespace trimmer
(require 'ws-trim)

(autoload 'whitespace-mode "whitespace-mode" 
  "Toggle whitespace mode.
	With arg, turn whitespace mode on iff arg is positive.
	In whitespace mode the different whitespaces (tab, blank return)
	are highlighted with different faces. The faces are:
	`whitespace-blank-face', `whitespace-tab-face' and 
	`whitespace-return-face'."
  t)

(autoload 'whitespace-incremental-mode "whitespace-mode" 
  "Toggle whitespace incremental mode.
     With arg, turn whitespace incremental mode on iff arg is positive.
	In whitespace incremental mode the different whitespaces (tab and 
	blank) are highlighted with different faces. The faces are:
	`whitespace-blank-face' and `whitespace-tab-face'.
	Use the command `whitespace-show-faces' to show their values.
	In this mode only these tabs and blanks are highlighted, which are in 
	the region from (point) - (window-heigh) to (point) + (window-heigh)."
  t)


;; trim modes
(add-hook 'c-mode-hook 'turn-on-ws-trim)
(add-hook 'c++-mode-hook 'turn-on-ws-trim)
(add-hook 'shell-script-mode 'turn-on-ws-trim)

;; whitespace checker
(require 'whitespace)

;; DocBook IDE mode
(autoload 'docbook-mode "docbookide" "Major mode for DocBook documents." t)

;; Turn on font lock when in DocBook mode
(add-hook 'docbook-mode-hook
          'turn-on-font-lock)

;; You might want to make this the default for .sgml or .xml documents,
;; or you might want to rely on -*- DocBook -*- on the first line,
;; or perhaps buffer variables. It's up to you...
;;(setq auto-mode-alist
;;      (append
;;       (list
;;      '("\\.sgm" . docbook-mode))
;;      '("\\.sgml" . docbook-mode))
;;      '("\\.xml" . docbook-mode))
;;       auto-mode-alist))


;; whitespace highliter
;; (require 'space-mode)
;; (space-mode 1)

;;
;; C/C++ Indentation styles
;;

(require 'cc-mode)

;; custom BSD indentation style
(setq c-style-alist
      (append '(("cBSD"
		 (c-basic-offset . 8)
		 (c-indent-level . 8)
		 (c-continued-statement-offset . 8)
		 (c-brace-offset . -8)
		 (c-label-offset . -8)
		 (c-offsets-alist . (
				     (comment-intro . 0)
				     (knr-argdecl-intro . +)
				     (knr-argdecl . 0)
				     (block-open . -)
				     (label . -1000)
				     (statement-cont . 4)
				     (arglist-cont . 4)
				     (arglist-cont-nonempty . 4)))))
	      c-style-alist))

;; XFMail indentation style
(setq c-style-alist
      (append '(("xfmail"
		 (c-basic-offset . 4)
		 (c-indent-level . 4)
		 (c-offsets-alist . (
				     (statement-cont . 4)
				     (arglist-cont . 4)
				     (arglist-cont-nonempty . 4)))
		 ))
	      c-style-alist))

(defun xfmail-load ()
  (setq-default tab-width 4)
;  (redisplay-buffer)
  (c-set-style "xfmail"))

;; Initializations for C-mode
(add-hook 'c-mode-hook
	  (function
	   (lambda ()
	     ;; Default
	     (c-set-style "cBSD")
	     (setq c-echo-syntactic-information-p t)

	     (if (string-match "xfmail" buffer-file-name)
		 (xfmail-load)))))

;; Initializations for C++-mode
(add-hook 'c++-mode-hook
	  (function
	   (lambda ()
	     (c-set-style "cBSD")
	     (setq c-echo-syntactic-information-p t)

;;	     (message buffer-file-name)
	     (if (string-match "xfmail" buffer-file-name)
		 (xfmail-load)))))

(setq-default c-default-style "cBSD")

;; yatex

; (add-to-list 'load-path "~/work/git_repo/xemacs/lisp/yatex1.72.zu")
; (require 'yatex)

; (add-hook 'yatex-mode-hook
; '(lambda() (setq indent-tabs-mode nil)))

; (setq auto-mode-alist (cons '("\\.tex$" . yatex-mode) auto-mode-alist))

;; python stuff

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

(setq py-indent-offset tab-width)
(setq python-mode-hook
'(lambda ()
"python mode hook override."
(setq tab-width 8)
(setq py-indent-offset 8)
))

;; thrift
(setq auto-mode-alist (cons '("\\.thrift$" . thrift-mode) auto-mode-alist))
(autoload 'thrift-mode "thrift" "Thrift editing mode." t)

;; sgml
;; (add-to-list 'load-path "/usr/lib/xemacs/xemacs-packages/lisp/sgml")

(setq auto-mode-alist (cons '("\\.sgml$" . sgml-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("sgml" . sgml-mode)
				   interpreter-mode-alist))

(autoload 'sgml-mode "sgml-mode" "SGML editing mode" t)

;; (require 'pycomplete)

;; ruby
;;
(add-to-list 'load-path "~/work/git_repo/xemacs/lisp/ruby-mode")
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files")
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
    				     interpreter-mode-alist))

(setq ruby-indent-level tab-width)

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
	     ))



;; auto-save?
(setq auto-save-directory (expand-file-name "~/work/git_repo/xemacs/autosave")
      auto-save-directory-fallback auto-save-directory
      auto-save-hash-p nil
      efs-auto-save t
      efs-auto-save-remotely nil
      ;; now that we have auto-save-timeout, let's crank this up
      ;; for better interactive response.
      auto-save-interval 500)

;; We load this afterwards because it checks to make sure the
;; auto-save-directory exists (creating it if not) when it's loaded.
(require 'auto-save)

(message "All done loading")
