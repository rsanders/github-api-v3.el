;;; github-api-v3-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (githubv3/send-pull-request) "github-api-v2-obsolete"
;;;;;;  "github-api-v2-obsolete.el" (20272 41010))
;;; Generated autoloads from github-api-v2-obsolete.el

(autoload 'githubv3/send-pull-request "github-api-v2-obsolete" "\
Send a pull request with text TEXT to RECIPIENTS.
RECIPIENTS should be a list of usernames.

\(fn TEXT RECIPIENTS)" nil nil)

;;;***

;;;### (autoloads (githubv3/delete-gist githubv3/fork-gist githubv3/get-gist
;;;;;;  githubv3/gists-for-user githubv3/repos-for-user githubv3/my-user-id
;;;;;;  githubv3/my-user-name githubv3/user-info githubv3/list-auth-tokens
;;;;;;  githubv3/create-auth-token githubv3/set-config githubv3/config)
;;;;;;  "github-api-v3" "github-api-v3.el" (20272 41089))
;;; Generated autoloads from github-api-v3.el

(autoload 'githubv3/config "github-api-v3" "\
Returns a GitHub specific value from the global Git config.

\(fn KEY)" nil nil)

(autoload 'githubv3/set-config "github-api-v3" "\
Sets a GitHub specific value to the global Git config.

\(fn KEY VALUE)" nil nil)

(autoload 'githubv3/create-auth-token "github-api-v3" "\
Create an authorization

\(fn)" nil nil)

(autoload 'githubv3/list-auth-tokens "github-api-v3" "\
Create an authorization

\(fn)" nil nil)

(autoload 'githubv3/user-info "github-api-v3" "\
Get info about a given user.

If a username not supplied, the authenticated user will be used. 

\(fn &optional USER)" nil nil)

(autoload 'githubv3/my-user-name "github-api-v3" "\
Return the authenticated user's name

\(fn)" nil nil)

(autoload 'githubv3/my-user-id "github-api-v3" "\
Return the authenticated user's ID

\(fn)" nil nil)

(autoload 'githubv3/repos-for-user "github-api-v3" "\
Return an array of all repos owned by USER.
The repos are decoded JSON objects (plists).

\(fn USER)" nil nil)

(autoload 'githubv3/gists-for-user "github-api-v3" "\
Return an array of all repos owned by USER.
The repos are decoded JSON objects (plists).

\(fn USER)" nil nil)

(autoload 'githubv3/get-gist "github-api-v3" "\
Return info about a named gist.
The result is a decoded JSON object (plists).

\(fn GIST)" nil nil)

(autoload 'githubv3/fork-gist "github-api-v3" "\
Fork a named gist and return the URL.

\(fn GIST)" nil nil)

(autoload 'githubv3/delete-gist "github-api-v3" "\
Delete a named gist and return the URL.

\(fn GIST)" nil nil)

(eval-after-load 'magit '(unless (featurep 'github-api-v3) (require 'github-api-v3)))

;;;***

;;;### (autoloads nil nil ("github-api-v3-localrepo.el" "github-api-v3-pkg.el"
;;;;;;  "github-api-v3-ui.el") (20274 6786 980093))

;;;***

(provide 'github-api-v3-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; github-api-v3-autoloads.el ends here
