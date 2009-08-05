
;;;### (autoloads (url-register-auth-scheme url-get-authentication)
;;;;;;  "url-auth" "url-auth.el" (15114 37017))
;;; Generated autoloads from url-auth.el

(autoload (quote url-get-authentication) "url-auth" "\
Return an authorization string suitable for use in the WWW-Authenticate
header in an HTTP/1.0 request.

URL    is the url you are requesting authorization to.  This can be either a
       string representing the URL, or the parsed representation returned by
       `url-generic-parse-url'
REALM  is the realm at a specific site we are looking for.  This should be a
       string specifying the exact realm, or nil or the symbol 'any' to
       specify that the filename portion of the URL should be used as the
       realm
TYPE   is the type of authentication to be returned.  This is either a string
       representing the type (basic, digest, etc), or nil or the symbol 'any'
       to specify that any authentication is acceptable.  If requesting 'any'
       the strongest matching authentication will be returned.  If this is
       wrong, its no big deal, the error from the server will specify exactly
       what type of auth to use
PROMPT is boolean - specifies whether to ask the user for a username/password
       if one cannot be found in the cache" nil nil)

(autoload (quote url-register-auth-scheme) "url-auth" "\
Register an HTTP authentication method.

TYPE     is a string or symbol specifying the name of the method.   This
         should be the same thing you expect to get returned in an Authenticate
         header in HTTP/1.0 - it will be downcased.
FUNCTION is the function to call to get the authorization information.  This
         defaults to `url-?-auth', where ? is TYPE
RATING   a rating between 1 and 10 of the strength of the authentication.
         This is used when asking for the best authentication for a specific
         URL.  The item with the highest rating is returned." nil nil)

;;;***

;;;### (autoloads (url-cache-expired url-cache-extract url-is-cached
;;;;;;  url-store-in-cache) "url-cache" "url-cache.el" (15105 46208))
;;; Generated autoloads from url-cache.el

(autoload (quote url-store-in-cache) "url-cache" "\
Store buffer BUFF in the cache." nil nil)

(autoload (quote url-is-cached) "url-cache" "\
Return non-nil if the URL is cached." nil nil)

(autoload (quote url-cache-extract) "url-cache" "\
Extract FNAM from the local disk cache" nil nil)

(autoload (quote url-cache-expired) "url-cache" "\
Return t iff a cached file has expired." nil nil)

;;;***

;;;### (autoloads (url-cid) "url-cid" "url-cid.el" (15092 11246))
;;; Generated autoloads from url-cid.el

(autoload (quote url-cid) "url-cid" nil nil nil)

;;;***

;;;### (autoloads (url-cookie-setup-save-timer url-cookie-handle-set-cookie
;;;;;;  url-cookie-retrieve url-cookie-write-file url-cookie-parse-file)
;;;;;;  "url-cookie" "url-cookie.el" (15090 47552))
;;; Generated autoloads from url-cookie.el

(autoload (quote url-cookie-parse-file) "url-cookie" nil nil nil)

(autoload (quote url-cookie-write-file) "url-cookie" nil nil nil)

(autoload (quote url-cookie-retrieve) "url-cookie" "\
Retrieves all the netscape-style cookies for a specified HOST and PATH" nil nil)

(autoload (quote url-cookie-handle-set-cookie) "url-cookie" nil nil nil)

(autoload (quote url-cookie-setup-save-timer) "url-cookie" "\
Reset the cookie saver timer." t nil)

;;;***

;;;### (autoloads (url-file) "url-file" "url-file.el" (15114 36962))
;;; Generated autoloads from url-file.el

(autoload (quote url-file) "url-file" "\
Handle file: and ftp: URLs." nil nil)

;;;***

;;;### (autoloads (url-open-stream url-gateway-nslookup-host) "url-gw"
;;;;;;  "url-gw.el" (15302 2626))
;;; Generated autoloads from url-gw.el

(autoload (quote url-gateway-nslookup-host) "url-gw" "\
Attempt to resolve the given HOST using nslookup if possible." t nil)

(autoload (quote url-open-stream) "url-gw" "\
Open a stream to HOST, possibly via a gateway.
Args per `open-network-stream'.
Will not make a connexion if `url-gateway-unplugged' is non-nil." nil nil)

;;;***

;;;### (autoloads (url-insert-file-contents url-file-local-copy url-copy-file
;;;;;;  url-setup-file-name-handlers) "url-handlers" "url-handlers.el"
;;;;;;  (15302 2986))
;;; Generated autoloads from url-handlers.el

(autoload (quote url-setup-file-name-handlers) "url-handlers" "\
Setup file-name handlers." nil nil)

(autoload (quote url-copy-file) "url-handlers" "\
Copy URL to NEWNAME.  Both args must be strings.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
Fourth arg KEEP-TIME non-nil means give the new file the same
last-modified time as the old one.  (This works on only some systems.)
A prefix arg makes KEEP-TIME non-nil." nil nil)

(autoload (quote url-file-local-copy) "url-handlers" "\
Copy URL into a temporary file on this machine.
Returns the name of the local copy, or nil, if FILE is directly
accessible." nil nil)

(autoload (quote url-insert-file-contents) "url-handlers" nil nil nil)

;;;***

;;;### (autoloads (url-history-save-history url-history-parse-history
;;;;;;  url-history-setup-save-timer) "url-history" "url-history.el"
;;;;;;  (15092 12080))
;;; Generated autoloads from url-history.el

(autoload (quote url-history-setup-save-timer) "url-history" "\
Reset the history list timer." t nil)

(autoload (quote url-history-parse-history) "url-history" "\
Parse a history file stored in FNAME." nil nil)

(autoload (quote url-history-save-history) "url-history" "\
Write the global history file into `url-history-file'.
The type of data written is determined by what is in the file to begin
with.  If the type of storage cannot be determined, then prompt the
user for what type to save as." t nil)

;;;***

;;;### (autoloads (url-http-file-attributes url-http-file-exists-p
;;;;;;  url-http) "url-http" "url-http.el" (15289 64199))
;;; Generated autoloads from url-http.el

(autoload (quote url-http) "url-http" "\
Retrieve URL via HTTP asynchronously.
URL must be a parsed URL.  See `url-generic-parse-url' for details.
When retrieval is completed, the function CALLBACK is executed with
CBARGS as the arguments." nil nil)

(autoload (quote url-http-file-exists-p) "url-http" nil nil nil)

(defalias (quote url-http-file-readable-p) (quote url-http-file-exists-p))

(autoload (quote url-http-file-attributes) "url-http" nil nil nil)

;;;***

;;;### (autoloads (url-irc) "url-irc" "url-irc.el" (14435 25453))
;;; Generated autoloads from url-irc.el

(autoload (quote url-irc) "url-irc" nil nil nil)

;;;***

;;;### (autoloads (url-ldap) "url-ldap" "url-ldap.el" (14398 30982))
;;; Generated autoloads from url-ldap.el

(autoload (quote url-ldap) "url-ldap" nil nil nil)

;;;***

;;;### (autoloads (url-mailto url-mail) "url-mailto" "url-mailto.el"
;;;;;;  (15293 59398))
;;; Generated autoloads from url-mailto.el

(autoload (quote url-mail) "url-mailto" nil t nil)

(autoload (quote url-mailto) "url-mailto" "\
Handle the mailto: URL syntax." nil nil)

;;;***

;;;### (autoloads (url-data url-generic-emulator-loader url-info
;;;;;;  url-man) "url-misc" "url-misc.el" (15105 46591))
;;; Generated autoloads from url-misc.el

(autoload (quote url-man) "url-misc" nil nil nil)

(autoload (quote url-info) "url-misc" nil nil nil)

(autoload (quote url-generic-emulator-loader) "url-misc" nil nil nil)

(defalias (quote url-rlogin) (quote url-generic-emulator-loader))

(defalias (quote url-telnet) (quote url-generic-emulator-loader))

(defalias (quote url-tn3270) (quote url-generic-emulator-loader))

(autoload (quote url-data) "url-misc" nil nil nil)

;;;***

;;;### (autoloads (url-snews url-news) "url-news" "url-news.el" (15114
;;;;;;  36876))
;;; Generated autoloads from url-news.el

(autoload (quote url-news) "url-news" nil nil nil)

(autoload (quote url-snews) "url-news" nil nil nil)

;;;***

;;;### (autoloads (url-ns-user-pref url-ns-prefs isInNet isResolvable
;;;;;;  dnsResolve dnsDomainIs isPlainHostName) "url-ns" "url-ns.el"
;;;;;;  (14913 8114))
;;; Generated autoloads from url-ns.el

(autoload (quote isPlainHostName) "url-ns" nil nil nil)

(autoload (quote dnsDomainIs) "url-ns" nil nil nil)

(autoload (quote dnsResolve) "url-ns" nil nil nil)

(autoload (quote isResolvable) "url-ns" nil nil nil)

(autoload (quote isInNet) "url-ns" nil nil nil)

(autoload (quote url-ns-prefs) "url-ns" nil nil nil)

(autoload (quote url-ns-user-pref) "url-ns" nil nil nil)

;;;***

;;;### (autoloads (url-generic-parse-url url-recreate-url) "url-parse"
;;;;;;  "url-parse.el" (15288 22758))
;;; Generated autoloads from url-parse.el

(autoload (quote url-recreate-url) "url-parse" nil nil nil)

(autoload (quote url-generic-parse-url) "url-parse" "\
Return a vector of the parts of URL.
Format is:
[proto username password hostname portnumber file reference attributes fullp]" nil nil)

;;;***

;;;### (autoloads (url-setup-privacy-info) "url-privacy" "url-privacy.el"
;;;;;;  (15293 59778))
;;; Generated autoloads from url-privacy.el

(autoload (quote url-setup-privacy-info) "url-privacy" nil t nil)

;;;***

;;;### (autoloads (url-view-url url-truncate-url-for-viewing url-file-extension
;;;;;;  url-hexify-string url-unhex-string url-parse-query-string
;;;;;;  url-basepath url-percentage url-display-percentage url-pretty-length
;;;;;;  url-strip-leading-spaces url-eat-trailing-space url-get-normalized-date
;;;;;;  url-lazy-message url-normalize-url url-insert-entities-in-string
;;;;;;  url-parse-args) "url-util" "url-util.el" (15288 22441))
;;; Generated autoloads from url-util.el

(autoload (quote url-parse-args) "url-util" nil nil nil)

(autoload (quote url-insert-entities-in-string) "url-util" "\
Convert HTML markup-start characters to entity references in STRING.
  Also replaces the \" character, so that the result may be safely used as
  an attribute value in a tag.  Returns a new string with the result of the
  conversion.  Replaces these characters as follows:
    &  ==>  &amp;
    <  ==>  &lt;
    >  ==>  &gt;
    \"  ==>  &quot;" nil nil)

(autoload (quote url-normalize-url) "url-util" "\
Return a 'normalized' version of URL.  This strips out default port
numbers, etc." nil nil)

(autoload (quote url-lazy-message) "url-util" "\
Just like `message', but is a no-op if called more than once a second.
Will not do anything if url-show-status is nil." nil nil)

(autoload (quote url-get-normalized-date) "url-util" "\
Return a 'real' date string that most HTTP servers can understand." nil nil)

(autoload (quote url-eat-trailing-space) "url-util" "\
Remove spaces/tabs at the end of a string." nil nil)

(autoload (quote url-strip-leading-spaces) "url-util" "\
Remove spaces at the front of a string." nil nil)

(autoload (quote url-pretty-length) "url-util" nil nil nil)

(autoload (quote url-display-percentage) "url-util" nil nil nil)

(autoload (quote url-percentage) "url-util" nil nil nil)

(autoload (quote url-basepath) "url-util" "\
Return the base pathname of FILE, or the actual filename if X is true" nil nil)

(autoload (quote url-parse-query-string) "url-util" nil nil nil)

(autoload (quote url-unhex-string) "url-util" "\
Remove %XXX embedded spaces, etc in a url.
If optional second argument ALLOW-NEWLINES is non-nil, then allow the
decoding of carriage returns and line feeds in the string, which is normally
forbidden in URL encoding." nil nil)

(autoload (quote url-hexify-string) "url-util" "\
Escape characters in a string" nil nil)

(autoload (quote url-file-extension) "url-util" "\
Return the filename extension of FNAME.  If optional variable X is t,
then return the basename of the file with the extension stripped off." nil nil)

(autoload (quote url-truncate-url-for-viewing) "url-util" "\
Return a shortened version of URL that is WIDTH characters or less wide.
WIDTH defaults to the current frame width." nil nil)

(autoload (quote url-view-url) "url-util" "\
View the current document's URL.  Optional argument NO-SHOW means
just return the URL, don't show it in the minibuffer." t nil)

;;;***

;;;### (autoloads nil "url" "url.el" (15132 64989))
;;; Generated autoloads from url.el

(defvar url-configuration-directory "~/.url")

;;;***

(provide 'url-auto)
