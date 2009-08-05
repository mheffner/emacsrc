;;; custom-load.el --- automatically extracted custom dependencies

;;; Code:

;old-cus-dep-hash: #s(hash-table test equal size 34 data ("/tmp/url/lisp/url-gw.el" ((url-gateway-nslookup-program . "url-gw") (url-gateway-broken-resolution . "url-gw") (url-gateway-telnet-password . "url-gw") (url-gateway-telnet-user-name . "url-gw") (url-gateway-telnet-password-prompt . "url-gw") (url-gateway-telnet-login-prompt . "url-gw") (url-gateway-telnet-parameters . "url-gw") (url-gateway-telnet-host . "url-gw") (url-gateway-rlogin-parameters . "url-gw") (url-gateway-rlogin-user-name . "url-gw") (url-gateway-rlogin-host . "url-gw") (url-gateway-prompt-pattern . "url-gw") (url-gateway-local-host-regexp . "url-gw") (url-gateway . "url-gw")) "/tmp/url/lisp/url-about.el" nil "/tmp/url/lisp/url-irc.el" ((url-irc-function . "url-irc")) "/tmp/url/lisp/url-file.el" nil "/tmp/url/lisp/url-cid.el" nil "/tmp/url/lisp/docomp.el" nil "/tmp/url/lisp/url-cookie.el" ((url-cookie-save-interval . "url-cookie") (url-cookie-untrusted-urls . "url-cookie") (url-cookie-trusted-urls . "url-cookie") (url-cookie-multiple-line . "url-cookie") (url-cookie-confirmation . "url-cookie") (url-cookie-file . "url-cookie") (url-cookie . "url-cookie")) "/tmp/url/lisp/url-https.el" nil "/tmp/url/lisp/url-ns.el" nil "/tmp/url/lisp/auto-autoloads.el" nil "/tmp/url/lisp/url-handlers.el" nil "/tmp/url/lisp/url-mailto.el" nil "/tmp/url/lisp/url-http.el" nil "/tmp/url/lisp/url.el" nil "/tmp/url/lisp/url-dired.el" nil "/tmp/url/lisp/url-cache.el" ((url-cache-creation-function . "url-cache") (url-cache-directory . "url-cache")) "/tmp/url/lisp/url-imap.el" nil "/tmp/url/lisp/url-parse.el" nil "/tmp/url/lisp/url-news.el" ((url-news . "url-news")) "/tmp/url/lisp/url-ftp.el" nil "/tmp/url/lisp/url-history.el" ((url-history-save-interval . "url-history") (url-history-file . "url-history") (url-history-track . "url-history") (url-history . "url-history")) "/tmp/url/lisp/url-nfs.el" nil "/tmp/url/lisp/url-util.el" ((url-debug . "url-util")) "/tmp/url/lisp/url-privacy.el" nil "/tmp/url/lisp/url-ldap.el" nil "/tmp/url/lisp/vc-dav.el" nil "/tmp/url/lisp/url-vars.el" ((url-gateway-method . "url-vars") (url-confirmation-func . "url-vars") (url-news-server . "url-vars") (url-show-status . "url-vars") (url-temporary-directory . "url-vars") (url-max-password-attempts . "url-vars") (url-mime-language-string . "url-vars") (url-bad-port-list . "url-vars") (url-standalone-mode . "url-vars") (url-passwd-entry-func . "url-vars") (url-proxy-services . "url-vars") (url-mail-command . "url-vars") (url-uncompressor-alist . "url-vars") (url-privacy-level . "url-vars") (url-directory-index-file . "url-vars") (url-personal-mail-address . "url-vars") (url-cache-expired . "url-vars") (url-automatic-caching . "url-vars") (url-honor-refresh-requests . "url-vars") (url-hairy . "url-vars") (url-mime . "url-vars") (url-cache . "url-vars") (url-file . "url-vars") (url . "url-vars")) "/tmp/url/lisp/url-auth.el" nil "/tmp/url/lisp/url-methods.el" nil "/tmp/url/lisp/url-auto.el" nil "/tmp/url/lisp/url-expand.el" nil "/tmp/url/lisp/url-dav.el" nil "/tmp/url/lisp/url-misc.el" nil "/tmp/url/lisp/url-proxy.el" nil))
(autoload 'custom-add-loads "cus-load")

(custom-add-loads 'hypermedia '("url-vars"))
(custom-add-loads 'url-gateway '("url-gw"))
(custom-add-loads 'url-hairy '("url-vars" "url-util"))
(custom-add-loads 'i18n '("url-vars"))
(custom-add-loads 'url-file '("url-vars" "url-cache" "url-cookie"))
(custom-add-loads 'url-cookie '("url-cookie"))
(custom-add-loads 'url-history '("url-history"))
(custom-add-loads 'url '("url-vars" "url-cookie" "url-gw" "url-history" "url-irc" "url-news"))
(custom-add-loads 'url-cache '("url-vars" "url-cache"))
(custom-add-loads 'url-mime '("url-vars"))

;;; custom-load.el ends here
