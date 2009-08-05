;;; url-handlers.el --- file-name-handler stuff for URL loading
;; Author: $Author: fx $
;; Created: $Date: 2001/10/11 21:14:18 $
;; Version: $Revision: 1.6 $
;; Keywords: comm, data, processes, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1999 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'url)
(require 'url-parse)
(require 'url-util)
(require 'mm-decode)
(require 'mailcap)

;;;###autoload
(defun url-setup-file-name-handlers ()
  "Setup file-name handlers."
  (cond
   ((not (boundp 'file-name-handler-alist))
    nil)				; Don't load if no alist
   ((rassq 'url-file-handler file-name-handler-alist)
    nil)				; Don't load twice
   (t
    (setq file-name-handler-alist
	  (let ((new-handler (cons
			      (concat "^/*"
				      (substring url-nonrelative-link 1 nil))
			      'url-file-handler)))
	    (if file-name-handler-alist
		(append (list new-handler) file-name-handler-alist)
	      (list new-handler)))))))

(defun url-file-handler (operation &rest args)
  "Function called from the `file-name-handler-alist' routines.
OPERATION is what needs to be done (`file-exists-p', etc).  ARGS are
the arguments that would have been passed to OPERATION."
  (let ((fn (or (get operation 'url-file-handlers)
		(intern-soft (format "url-%s" operation))))
	(url (car args))
	(myargs (cdr args)))
    (if (and fn (fboundp fn))
	(progn
	  (if (= (string-to-char url) ?/)
	      (setq url (substring url 1 nil)))
	  (apply fn url myargs))
      (let (file-name-handler-alist)
	(apply operation url myargs)))))

(defun url-file-handler-identity (&rest args)
  ;; Identity function
  (car args))

;; These are operations that we can fully support
(put 'file-readable-p 'url-file-handlers 'url-file-exists-p)
(put 'substitute-in-file-name 'url-file-handlers 'url-file-handler-identity)

;; The actual implementation
;;;###autoload
(defun url-copy-file (url newname &optional ok-if-already-exists keep-time)
  "Copy URL to NEWNAME.  Both args must be strings.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
Fourth arg KEEP-TIME non-nil means give the new file the same
last-modified time as the old one.  (This works on only some systems.)
A prefix arg makes KEEP-TIME non-nil."
  (if (and (file-exists-p newname)
	   (not ok-if-already-exists))
      (error "Opening output file: File already exists, %s" newname))
  (let ((buffer (url-retrieve-synchronously url))
	(handle nil))
    (if (not buffer)
	(error "Opening input file: No such file or directory, %s" url))
    (save-excursion
      (set-buffer buffer)
      (setq handle (mm-dissect-buffer t)))
    (mm-save-part-to-file handle newname)
    (kill-buffer buffer)
    (mm-destroy-parts handle)))

;;;###autoload
(defun url-file-local-copy (url &rest ignored)
  "Copy URL into a temporary file on this machine.
Returns the name of the local copy, or nil, if FILE is directly
accessible."
  (let ((filename (make-temp-name "url")))
    (url-copy-file url filename)
    filename))

;;;###autoload
(defun url-insert-file-contents (url &optional visit beg end replace)
  (let ((buffer (url-retrieve-synchronously url))
	(handle nil)
	(data nil))
    (if (not buffer)
	(error "Opening input file: No such file or directory, %s" url))
    (save-excursion
      (set-buffer buffer)
      (setq handle (mm-dissect-buffer t))
      (set-buffer (mm-handle-buffer handle))
      (if beg
	  (setq data (buffer-substring beg end))
	(setq data (buffer-string))))
    (kill-buffer buffer)
    (mm-destroy-parts handle)
    (if replace (delete-region (point-min) (point-max)))
    (insert data)
    (list url (length data))))

(defun url-file-name-completion (url directory)
  (error "Unimplemented"))

(defun url-file-name-all-completions (file directory)
  (error "Unimplemented"))

;; All other handlers map onto their respective backends.
(defmacro url-handlers-create-wrapper (method args)
  (` (defun (, (intern (format "url-%s" method))) (, args)
       (, (format "URL file-name-handler wrapper for `%s' call.\n---\n%s" method
		  (or (documentation method t) "No original documentation.")))
       (setq url (url-generic-parse-url url))
       (funcall (url-scheme-get-property (url-type url) (quote (, method)))
		(,@ (remove '&rest (remove '&optional args)))))))

(url-handlers-create-wrapper file-exists-p (url))
(url-handlers-create-wrapper file-attributes (url))
(url-handlers-create-wrapper file-symlink-p (url))
(url-handlers-create-wrapper file-writable-p (url))
(url-handlers-create-wrapper file-directory-p (url))
(url-handlers-create-wrapper file-executable-p (url))

(if (featurep 'xemacs)
    (progn
      ;; XEmacs specific prototypes
      (url-handlers-create-wrapper
       directory-files (url &optional full match nosort files-only))
      (url-handlers-create-wrapper
       file-truename (url &optional default)))
  ;; Emacs specific prototypes
  (url-handlers-create-wrapper
   directory-files (url &optional full match nosort))
  (url-handlers-create-wrapper
   file-truename (url &optional counter prev-dirs)))

(provide 'url-handlers)
