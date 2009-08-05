;;; url-misc.el --- Misc Uniform Resource Locator retrieval code
;; Author: $Author: fx $
;; Created: $Date: 2001/05/15 23:04:31 $
;; Version: $Revision: 1.4 $
;; Keywords: comm, data, processes

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

(require 'url-vars)
(require 'url-parse)
(autoload 'Info-goto-node "info" "" t)

;;;###autoload
(defun url-man (url)
  (require 'man)
  (man (url-filename url))
  nil)

;;;###autoload
(defun url-info (url)
  ;; Fetch an info node
  (let* ((fname (url-filename url))
	 (node (url-unhex-string (or (url-target url) "Top"))))
    (if (and fname node)
	(Info-goto-node (concat "(" fname ")" node))
      (error "Malformed url: %s" (url-recreate-url url)))
    nil))

(defun url-do-terminal-emulator (type server port user)
  (terminal-emulator
   (generate-new-buffer (format "%s%s" (if user (concat user "@") "") server))
   (case type
     (rlogin "rlogin")
     (telnet "telnet")
     (tn3270 "tn3270")
     (otherwise
      (error "Unknown terminal emulator required: %s" type)))
   (case type
     (rlogin
      (if user
	  (list server "-l" user)
	(list server)))
     (telnet
      (if user (message "Please log in as user: %s" user))
      (if port
	  (list server port)
	(list server)))
     (tn3270
      (if user (message "Please log in as user: %s" user))
      (list server)))))

;;;###autoload
(defun url-generic-emulator-loader (url)
  (let* ((type (intern (downcase (url-type url))))
	 (server (url-host url))
	 (name (url-user url))
	 (port (url-port url)))
    (url-do-terminal-emulator type server port name))
  nil)

;;;###autoload
(defalias 'url-rlogin 'url-generic-emulator-loader)
;;;###autoload
(defalias 'url-telnet 'url-generic-emulator-loader)
;;;###autoload
(defalias 'url-tn3270 'url-generic-emulator-loader)

;; ftp://ietf.org/internet-drafts/draft-masinter-url-data-02.txt
;;;###autoload
(defun url-data (url)
  (let ((content-type nil)
	(desc (url-filename url))
	(encoding nil)
	(data nil))
    (save-excursion
      (if (not (string-match "\\([^;,]*\\);*\\([^,]*\\)," desc))
	  (error "Malformed data URL: %s" desc)
	(setq content-type (match-string 1 desc)
	      encoding (match-string 2 desc)
	      data (url-unhex-string (substring desc (match-end 0)))))
      (set-buffer (generate-new-buffer " *url-data*"))
      (mm-disable-multibyte)
      (if (= 0 (length content-type)) (setq content-type "text/plain"))
      (if (= 0 (length encoding)) (setq encoding "8bit"))
      (insert (format "Content-Length: %d\n" (length data))
	      "Content-Type: " content-type "\n"
	      "Content-Encoding: " encoding "\n"
	      "\n")
      (if data (insert data))
      (current-buffer))))

(provide 'url-misc)
