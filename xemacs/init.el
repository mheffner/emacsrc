
(add-to-list 'load-path "/home/spock/emacs/lisp")

;; key/mouse bindings

;; wheel mouse
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)

;; random binding
(define-key global-map [(f1)] 'delete-other-windows)
(define-key global-map [(f5)] 'buffer-menu)
(define-key global-map [(f6)] 'fume-list-functions)

;; paragraph fill
(global-set-key [(meta q)] 'fill-paragraph)

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
;; (setq-default user-mail-address (quote mheffner@vt.edu))
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

;; SML mode
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/sml-mode")
;; (load "sml-mode-startup")


;; xcscope
(require 'xcscope)

;; function menu
(require 'func-menu)

(add-to-list 'load-path "/usr/share/emacs/site-lisp")

;; whitespace trimmer
(require 'ws-trim)

;; trim modes
(add-hook 'c-mode-hook 'turn-on-ws-trim)
(add-hook 'c++-mode-hook 'turn-on-ws-trim)
(add-hook 'shell-script-mode 'turn-on-ws-trim)

;; whitespace checker
(require 'whitespace)

;; whitespace highliter
;; (require 'space-mode)
;; (space-mode 1)

(require 'actionscript-mode)


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

;; yatex
; (add-to-list 'load-path "/home/spock/emacs/lisp/yatex")
; (require 'yatex)
; (add-hook 'yatex-mode-hook
;   '(lambda() (setq indent-tabs-mode nil)))
; (setq YaTeX-use-font-lock t)
; (setq auto-mode-alist
;      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
; (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

; (add-hook 'yatex-mode-hook
;	  '(lambda() (setq indent-tabs-mode nil)))

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
)
)


; cscope
;(require 'xcscope)

;; auto-save?
(setq auto-save-directory (expand-file-name "~/emacs/autosave/")
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
