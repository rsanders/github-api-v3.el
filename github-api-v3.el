;;; githubv3.el --- implementation of GitHub API v3

;; Copyright (c) 2012 Robert Sanders
;; Copyright (c) 2010 Nathan Weizenbaum
;; Licensed under the same terms as Emacs.

;; Author: Robert Sanders
;; URL: http://github.com/rsanders/github-api-v3.el
;; Version: 0.1
;; Created: 2012-02-05
;; By: Nathan Weizenbaum
;; Keywords: git, github, magit, api
;; Package-Requires: ((magit "0.8") (json "1.2"))

;; Borrowing *heavily* from the partial GitHub v2 API included in Magithub:

;; Author: Nathan Weizenbaum
;; URL: http://github.com/nex3/magithub
;; Version: 0.1
;; Created: 2010-06-06
;; By: Nathan Weizenbaum

;;; Commentary:

;; This package does two things uses Magit's excellent Git library
;; to build an Elisp library for interfacing with v3 of GitHub's API.

(require 'magit)
(require 'url)
(require 'json)
(require 'crm)
(eval-when-compile (require 'cl))


;;; Variables

(defvar githubv3/api-base "https://api.github.com/"
  "The base URL for accessing the GitHub API.")

(defvar githubv3/github-url "https://github.com/"
  "The URL for the main GitHub site.

This is used for some calls that aren't supported by the official API.")

(defvar githubv3/use-ssl t
  "If non-nil, access GitHub via HTTPS.
This is more secure, but slower.")

(defvar githubv3/gist-url "https://gist.github.com/"
  "The URL for the Gist site.")

(defvar githubv3/view-gist t
  "Whether or not to open new Gists in the browser.")

(defvar githubv3/request-data nil
  "An assoc list of parameter names to values.

This is meant to be dynamically bound around `githubv3/retrieve'
and `githubv3/retrieve-synchronously'.")

(defvar githubv3/parse-response t
  "Whether to parse responses from GitHub as JSON.
Used by `githubv3/retrieve' and `githubv3/retrieve-synchronously'.
This should only ever be `let'-bound, not set outright.")

(defvar githubv3/users-history nil
  "A list of users selected via `githubv3/read-user'.")

(defvar githubv3/repos-history nil
  "A list of repos selected via `githubv3/read-repo'.")

(defvar githubv3/-repo-obj-cache (make-hash-table :test 'equal)
  "A hash from (USERNAME . REPONAME) to decoded JSON repo objects (plists).
This caches the result of `githubv3/repo-obj' and
`githubv3/cached-repo-obj'.")


;;; Utilities

(defun githubv3/-remove-if (predicate seq)
  "Remove all items satisfying PREDICATE from SEQ.
Like `remove-if', but without the cl runtime dependency."
  (loop for el being the elements of seq
        if (not (funcall predicate el)) collect el into els
        finally return els))

(defun githubv3/-position (item seq)
  "Return the index of ITEM in SEQ.
Like `position', but without the cl runtime dependency.

Comparison is done with `eq'."
  (loop for el in seq until (eq el item) count t))

(defun githubv3/-cache-function (fn)
  "Return a lambda that will run FN but cache its return values.
The cache is a very naive assoc from arguments to returns.
The cache will only last as long as the lambda does.

FN may call githubv3/-use-cache, which will use a pre-cached
value if available or recursively call FN if not."
  (lexical-let ((fn fn) cache cache-fn)
    (setq cache-fn
          (lambda (&rest args)
            (let ((cached (assoc args cache)))
              (if cached (cdr cached)
                (flet ((githubv3/-use-cache (&rest args) (apply cache-fn args)))
                  (let ((val (apply fn args)))
                    (push (cons args val) cache)
                    val))))))))

(defun githubv3/make-query-string (params)
  "Return a query string constructed from PARAMS.
PARAMS is an assoc list of parameter names to values.

Any parameters with a nil values are ignored."
  (replace-regexp-in-string
   "&+" "&"
   (mapconcat
    (lambda (param)
      (when (cdr param)
        (concat (url-hexify-string (car param)) "="
                (url-hexify-string (cdr param)))))
    params "&")))

(defun githubv3/parse-repo (repo)
  "Parse a REPO string of the form \"username/repo\".
Return (USERNAME . REPO), or raise an error if the format is
incorrect."
  (condition-case err
      (destructuring-bind (username repo) (split-string repo "/")
        (cons username repo))
    (wrong-number-of-arguments (error "Invalid GitHub repository %s" repo))))

(defun githubv3/repo-url (username repo &optional sshp)
  "Return the repository URL for USERNAME/REPO.
If SSHP is non-nil, return the SSH URL instead.  Otherwise,
return the HTTP URL."
  (format (if sshp "git@github.com:%s/%s.git" "http://github.com/%s/%s.git")
          username repo))

(defun githubv3/remote-info (remote)
  "Return (USERNAME REPONAME SSHP) for the given REMOTE.
Return nil if REMOTE isn't a GitHub remote.

USERNAME is the owner of the repo, REPONAME is the name of the
repo, and SSH is non-nil if it's checked out via SSH."
  (block nil
    (let ((url (magit-get "remote" remote "url")))
      (unless url (return))
      (when (string-match "\\(?:git\\|http\\)://github\\.com/\\(.*?\\)/\\(.*\\)\.git" url)
        (return (list (match-string 1 url) (match-string 2 url) nil)))
      (when (string-match "git@github\\.com:\\(.*?\\)/\\(.*\\)\\.git" url)
        (return (list (match-string 1 url) (match-string 2 url) t)))
      (return))))

(defun githubv3/remote-for-commit (commit)
  "Return the name of the remote that contains COMMIT.
If no remote does, return nil.  COMMIT should be the full SHA1
commit hash.

If origin contains the commit, it takes precedence.  Otherwise
the priority is nondeterministic."
  (flet ((name-rev (remote commit)
                   (magit-git-string "name-rev" "--name-only" "--no-undefined" "--refs"
                                     ;; I'm not sure why the initial * is required,
                                     ;; but if it's not there this always returns nil
                                     (format "*remotes/%s/*" remote) commit)))
    (let ((remote (or (name-rev "origin" commit) (name-rev "*" commit))))
      (when (and remote (string-match "^remotes/\\(.*?\\)/" remote))
        (match-string 1 remote)))))

(defun githubv3/remote-info-for-commit (commit)
  "Return information about the GitHub repo for the remote that contains COMMIT.
If no remote does, return nil.  COMMIT should be the full SHA1
commit hash.

The information is of the form returned by `githubv3/remote-info'.

If origin contains the commit, it takes precedence.  Otherwise
the priority is nondeterministic."
  (let ((remote (githubv3/remote-for-commit commit)))
    (when remote (githubv3/remote-info remote))))

(defun githubv3/branches-for-remote (remote)
  "Return a list of branches in REMOTE, as of the last fetch."
  (let ((lines (magit-git-lines "remote" "show" "-n" remote)) branches)
    (while (not (string-match-p "^  Remote branches:" (pop lines)))
      (unless lines (error "Unknown output from `git remote show'")))
    (while (string-match "^    \\(.*\\)" (car lines))
      (push (match-string 1 (pop lines)) branches))
    branches))

(defun githubv3/repo-relative-path ()
  "Return the path to the current file relative to the repository root.
Only works within `githubv3/minor-mode'."
  (let ((filename buffer-file-name))
    (with-current-buffer githubv3/status-buffer
      (file-relative-name filename default-directory))))

(defun githubv3/name-rev-for-remote (rev remote)
  "Return a human-readable name for REV that's valid in REMOTE.
Like `magit-name-rev', but sanitizes things referring to remotes
and errors out on local-only revs."
  (setq rev (magit-name-rev rev))
  (if (and (string-match "^\\(remotes/\\)?\\(.*?\\)/\\(.*\\)" rev)
           (equal (match-string 2 rev) remote))
      (match-string 3 rev)
    (unless (githubv3/remote-contains-p remote rev)
      (error "Commit %s hasn't been pushed"
             (substring (magit-git-string "rev-parse" rev) 0 8)))
    (cond
     ;; Assume the GitHub repo will have all the same tags as we do,
     ;; since we can't actually check without performing an HTTP request.
     ((string-match "^tags/\\(.*\\)" rev) (match-string 1 rev))
     ((and (not (string-match-p "^remotes/" rev))
           (member rev (githubv3/branches-for-remote remote))
           (githubv3/ref= rev (concat remote "/" rev)))
      rev)
     (t (magit-git-string "rev-parse" rev)))))

(defun githubv3/remotes-containing-ref (ref)
  "Return a list of remotes containing REF."
  (loop with remotes
        for line in (magit-git-lines "branch" "-r" "--contains" ref)
        if (and (string-match "^ *\\(.+?\\)/" line)
                (not (string= (match-string 1 line) (car remotes))))
        do (push (match-string 1 line) remotes)
        finally return remotes))

(defun githubv3/remote-contains-p (remote ref)
  "Return whether REF exists in REMOTE, in any branch.
This does not fetch origin before determining existence, so it's
possible that its result is based on stale data."
  (member remote (githubv3/remotes-containing-ref ref)))

(defun githubv3/ref= (ref1 ref2)
  "Return whether REF1 refers to the same commit as REF2."
  (string= (magit-rev-parse ref1) (magit-rev-parse ref2)))


;;; Reading Input

(defun githubv3/-lazy-completion-callback (fn &optional noarg)
  "Converts a simple string-listing FN into a lazy-loading completion callback.
FN should take a string (the contents of the minibuffer) and
return a list of strings (the candidates for completion).  This
method takes care of any caching and makes sure FN isn't called
until completion needs to happen.

If NOARG is non-nil, don't pass a string to FN."
  (lexical-let ((fn (githubv3/-cache-function fn)) (noarg noarg))
    (lambda (string predicate allp)
      (let ((strs (if noarg (funcall fn) (funcall fn string))))
        (if allp (all-completions string strs predicate)
          (try-completion string strs predicate))))))

;;;
;;; NOT CONVERTED TO V3 YET
;;;

(defun githubv3/read-user (&optional prompt predicate require-match initial-input
                                     hist def inherit-input-method)
  "Read a GitHub username from the minibuffer with completion.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub user: \".  HIST defaults to
'githubv3/users-history.

WARNING: This function currently doesn't work fully, since
GitHub's user search API only returns an apparently random subset
of users."
  (setq hist (or hist 'githubv3/users-history))
  (completing-read (or prompt "GitHub user: ")
                   (githubv3/-lazy-completion-callback
                    (lambda (s)
                      (mapcar (lambda (user) (plist-get user :name))
                              (githubv3/user-search s))))
                   predicate require-match initial-input hist def inherit-input-method))

;;;
;;; NOT CONVERTED TO V3 YET
;;;

(defun githubv3/read-repo-for-user (user &optional prompt predicate require-match
                                         initial-input hist def inherit-input-method)
  "Read a GitHub repository from the minibuffer with completion.
USER is the owner of the repository.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub repo: <user>/\"."
  (lexical-let ((user user))
    (completing-read (or prompt (concat "GitHub repo: " user "/"))
                     (githubv3/-lazy-completion-callback
                      (lambda ()
                        (mapcar (lambda (repo) (plist-get repo :name))
                                (githubv3/repos-for-user user)))
                      'noarg)
                     predicate require-match initial-input hist def
                     inherit-input-method)))

;;;
;;; NOT CONVERTED TO V3 YET
;;;

(defun githubv3/read-repo (&optional prompt predicate require-match initial-input
                                     hist def inherit-input-method)
  "Read a GitHub user-repository pair with completion.
Return (USERNAME . REPO), or nil if the user enters no input.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub repo (user/repo): \".  HIST defaults to
'githubv3/repos-history.  If REQUIRE-MATCH is non-nil and the
user enters no input, raises an error.

WARNING: This function currently doesn't work fully, since
GitHub's user search API only returns an apparently random subset
of users, and also has no way to search for users whose names
begin with certain characters."
  (setq hist (or hist 'githubv3/repos-history))
  (let ((result (completing-read
                 (or prompt "GitHub repo (user/repo): ")
                 (githubv3/-lazy-completion-callback 'githubv3/-repo-completions)
                 predicate require-match initial-input hist def inherit-input-method)))
    (if (string= result "")
        (when require-match (error "No repository given"))
      (githubv3/parse-repo result))))

(defun githubv3/-repo-completions (string)
  "Try completing the given GitHub user/repository pair.
STRING is the text already in the minibuffer, PREDICATE is a
predicate that the string must satisfy."
  (destructuring-bind (username . rest) (split-string string "/")
    (if (not rest) ;; Need to complete username before we start completing repo
        (mapcar (lambda (user) (concat (plist-get user :name) "/"))
                (githubv3/user-search username))
      (if (not (string= (car rest) ""))
          (githubv3/-use-cache (concat username "/"))
        (mapcar (lambda (repo) (concat username "/" (plist-get repo :name)))
                (githubv3/repos-for-user username))))))

;;;
;;; NOT CONVERTED TO V3 YET
;;;

(defun githubv3/read-pull-request-recipients ()
  "Read a list of recipients for a GitHub pull request."
  (let ((collabs (githubv3/repo-parent-collaborators))
        (network (githubv3/repo-network)))
    (githubv3/-remove-if
     (lambda (s) (string= s ""))
     (completing-read-multiple
      "Send pull request to: "
      (mapcar (lambda (repo) (plist-get repo :owner)) (githubv3/repo-network))
      nil nil (concat (mapconcat 'identity collabs crm-separator)
                      (if (= (length collabs) (length network)) "" crm-separator))))))

;;;
;;; NOT CONVERTED TO V3 YET
;;;

(defun githubv3/read-untracked-fork ()
  "Read the name of a fork of this repo that we aren't yet tracking.
This will accept either a username or a username/repo pair,
and return (USERNAME . REPONAME)."
  (let ((fork
         (completing-read
          "Track fork (user or user/repo): "
          (githubv3/-lazy-completion-callback
           (lambda ()
             (mapcar (lambda (repo) (concat (plist-get repo :owner) "/"
                                            (plist-get repo :name)))
                     (githubv3/untracked-forks)))
           'noarg)
          nil nil nil 'githubv3/repos-history)))
    (cond
     ((string= fork "") (error "No fork given"))
     ((string-match "/" fork) (githubv3/parse-repo fork))
     (t (cons fork (githubv3/repo-name))))))

;;; Requests

(defun magit-request-url (path)
  "Return the full GitHub URL for the resource PATH.

PATH can either be a string or a list of strings.
In the latter case, they're URL-escaped and joined with \"/\".

If `url-request-method' is GET, the returned URL will include
`url-request-data' as the query string."
  (let ((url
         (concat githubv3/api-base
                 (if (stringp path) path (mapconcat 'url-hexify-string path "/"))
                 (if (string= url-request-method "GET")
                     (concat "?" url-request-data)
                   ""))))
    (if githubv3/use-ssl url
      (replace-regexp-in-string "^https" "http" url))))

(defvar githubv3/auth-type "token"
  "Set to 'token' or 'basic' to determine auth type used")

;; v3
(defmacro githubv3/with-auth (&rest body)
  `(if (equal githubv3/auth-type "basic")
       (githubv3/with-basic-auth
         ,@body)
     (githubv3/with-token-auth
      ,@body)))

;; v3
(defmacro githubv3/with-token-auth (&rest body)
  "Runs BODY with GitHub token authorization in an header."
  (declare (indent 0))
  (let ((auth (gensym)))
    `(let* ((,auth (githubv3/auth-info))
            (url-request-extra-headers (append (list
                                                (cons "Authorization"
                                                      (concat "token " (cdr ,auth))))
                                               url-request-extra-headers)))
       ,@body)))

;; v3
(defmacro githubv3/with-basic-auth (&rest body)
  "Runs BODY with GitHub basic authorization taken from username/password in a header."
  (declare (indent 0))
  (let ((auth (gensym)))
    `(let* ((,auth (githubv3/basic-auth-info))
            (url-request-extra-headers (append (list
                                                (cons "Authorization"
                                                      (concat "Basic "
                                                               (base64-encode-string
                                                                (concat (car ,auth) ":" (cdr ,auth))))))
                                               url-request-extra-headers)))
       ,@body)))

(defun github-auth-info ()
  "Returns the user's GitHub authorization information.
Searches for a GitHub username and token in the global git config,
and returns (USERNAME . TOKEN). If nothing is found, prompts
for the info then sets it to the git config."
  (interactive)
  
  ;; If we've been called within a scope that already has this
  ;; defined, don't take the time to get it again.
  (if (boundp '*github-auth-info*)
      *github-auth-info*
    
    (let* ((user (or github-user (github-config "user")))
           (token (or github-token (github-config "v3token"))))
      
      (when (not user)
        (setq user (read-string "GitHub username: "))
        (github-set-config "user" user))
      
      (when (not token)
        (setq token (read-string "GitHub password: "))
        (github-set-config "password" token))
      
      (cons user token))))


(defun githubv3/handle-errors (status)
  "Handle any errors reported in a `url-retrieve' callback.
STATUS is the first argument passed to the callback.

If there is an error and GitHub returns an error message, that
message is printed with `error'.  Otherwise, the HTTP error is
signaled."
  (loop for (name val) on status by 'cddr
        do (when (eq name :error)
             (if (not githubv3/handle-errors)
                 (signal (var val) (cdr val))
               (condition-case err
                   (let* ((json-object-type 'plist)
                          (data (json-read))
                          (err (plist-get data :error)))
                     (unless err (signal 'json-readtable-error nil))
                     (error "GitHub error: %s" err))
                 (json-readtable-error (signal (car val) (cdr val))))))))

;; v3
(defun githubv3/retrieve (path callback &optional cbargs)
  "Retrieve GitHub API PATH asynchronously.
Call CALLBACK with CBARGS when finished.

PATH can either be a string or a list of strings.
In the latter case, they're URL-escaped and joined with \"/\".

Like `url-retrieve', except for the following:
* PATH is an API resource path, not a full URL.
* GitHub authorization is automatically enabled.
* `githubv3/request-data' is used instead of `url-request-data'.
* CALLBACK is passed a decoded JSON object (as a plist) rather
  than a list of statuses.  Basic error handling is done by `githubv3/retrieve'.

If `githubv3/parse-response' is nil, CALLBACK is just passed nil
rather than the JSON response object."
  (githubv3/with-auth
    (let (
          (url-mime-accept-string "application/json")
          ;; (url-request-extra-headers (append url-request-extra-headers (list '("Accept" . "application/json"))))
          )
      (lexical-let ((callback callback) (githubv3/parse-response githubv3/parse-response))
        (url-retrieve (magit-request-url path)
                      (lambda (status &rest cbargs)
                        (when githubv3/parse-response
                          (search-forward "\n\n" nil t)) ; Move past headers
                        (githubv3/handle-errors status)
                        (apply callback
                               (if (not githubv3/parse-response)
                                   (current-buffer)
                                 (let* ((json-object-type 'plist)
                                        (obj (json-read)))
                                   (kill-buffer)
                                   obj))
                               cbargs))
                      cbargs)))))

;; v3
(defun githubv3/retrieve-synchronously (path)
  "Retrieve GitHub API PATH synchronously.

PATH can either be a string or a list of strings.
In the latter case, they're URL-escaped and joined with \"/\".

Like `url-retrieve-synchronously', except for the following:
* PATH is an API resource path, not a full URL.
* GitHub authorization is automatically enabled.
* `githubv3/request-data' is used instead of `url-request-data'.
* Return a decoded JSON object (as a plist) rather than a buffer
  containing the response unless `githubv3/parse-response' is nil."
  (githubv3/with-auth
    (let (
          (url-mime-accept-string "application/json")
          )
      (with-current-buffer (url-retrieve-synchronously (magit-request-url path))
        (goto-char (point-min))
        (if (not githubv3/parse-response) (current-buffer)
          (search-forward "\n\n" nil t) ; Move past headers
          (let* ((data (let ((json-object-type 'plist)) (json-read)))
                 (err (plist-get data :error)))
            (when err (error "GitHub error: %s" err))
            (kill-buffer)
            data))))))


;;; Configuration
;; This API was taken from gist.el (http://github.com/defunkt/gist.el),
;; and renamed to avoid conflict.  The code also uses Magit rather
;; than relying on the Git executable directly.

(defun githubv3/config (key)
  "Returns a GitHub specific value from the global Git config."
  (magit-git-string "config" "--global" (concat "github." key)))


(defun githubv3/set-config (key value)
  "Sets a GitHub specific value to the global Git config."
  (magit-git-string "config" "--global" (concat "github." key) value))


(defun githubv3/auth-info ()
  "Returns the user's GitHub authorization information.
Searches for a GitHub username and token in the global git config,
and returns (USERNAME . TOKEN). If nothing is found, prompts
for the info then sets it to the git config."
  (interactive)
  
  (let* ((user (githubv3/config "user"))
         (token (githubv3/config "v3token")))
    
    (when (not user)
      (setq user (read-string "GitHub username: "))
      (githubv3/set-config "user" user))
    
    (when (not token)
      (setq token (read-string "GitHub API token: "))
      (githubv3/set-config "v3token" token))
    
    (cons user token)))

;; v3
(defun githubv3/basic-auth-info ()
  "Returns the user's GitHub basic authorization information.
Searches for a GitHub username and password in the global git config,
and returns (USERNAME . PASSWORD). If nothing is found, prompts
for the info."
  (interactive)
  
  (let* ((user (githubv3/config "user"))
         (password (githubv3/config "password")))
    
    (when (not user)
      (setq user (read-string "GitHub username: "))
      (githubv3/set-config "user" user))
    
    (when (not password)
      (setq password (read-string "GitHub password: ")))
    
    (cons user password)))


;;; OAuth

;; v3
;;;###autoload
(defun githubv3/create-auth-token ()
  "Create an authorization"
  (let ((url-request-method "POST")
        (url-request-data "{\"scopes\": [\"user\", \"public_repo\", \"repo\", \"gist\"]}")
        (githubv3/auth-type "basic"))
    (githubv3/retrieve-synchronously
     (list "authorizations"))
    ))

;; v3
;;;###autoload
(defun githubv3/list-auth-tokens ()
  "Create an authorization"
  (let ((url-request-method "GET")
        (githubv3/auth-type "basic"))
    (githubv3/retrieve-synchronously
     (list "authorizations"))
    ))

;; v3
;;;###autoload
(defun githubv3/user-info (&optional user)
  "Get info about"
  (let ((url-request-method "GET"))
    (githubv3/retrieve-synchronously
     (if user (list "users" user)
       (list "user"))
    )))


;;; Gists

;; v3
;;;###autoload
(defun githubv3/gists-for-user (user)
  "Return an array of all repos owned by USER.
The repos are decoded JSON objects (plists)."
  (let ((url-request-method "GET"))
    (githubv3/retrieve-synchronously
      (list "users" user "gists"))
     ))

;; v3
;;;###autoload
(defun githubv3/get-gist (gist)
  "Return info about a named gist.
The result is a decoded JSON object (plists)."
  (let ((url-request-method "GET"))
    (githubv3/retrieve-synchronously
     (list "gists" gist))
    ))

;; v3
;;;###autoload
(defun githubv3/fork-gist (gist)
  "Fork a named gist and return the URL."
  (let ((url-request-method "POST"))
    (githubv3/retrieve-synchronously
     (list "gists" gist "fork"))
    ))

;; v3
;;;###autoload
(defun githubv3/delete-gist (gist)
  "Delete a named gist and return the URL."
  (let ((url-request-method "DELETE"))
    (githubv3/retrieve-synchronously
     (list "gists" gist))
    ))



;;; GitHub Information

;; v3
;;;###autoload
(defun githubv3/repos-for-user (user)
  "Return an array of all repos owned by USER.
The repos are decoded JSON objects (plists)."
  (let ((url-request-method "GET"))
    (plist-get
     (githubv3/retrieve-synchronously
      (list "repos" "show" user))
     :repositories)))

;;;
;;; NOT CONVERTED TO V3 YET - may not be supported
;;;

;; (defun githubv3/user-search (user)
;;   "Run a GitHub user search for USER.
;; Return an array of all matching users.

;; WARNING: WARNING: This function currently doesn't work fully,
;; since GitHub's user search API only returns an apparently random
;; subset of users."
;;   (if (string= user "") []
;;     (let ((url-request-method "GET"))
;;       (plist-get
;;        (githubv3/retrieve-synchronously
;;         (list "user" "search" user))
;;        :users))))

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/repo-obj (&optional username repo)
  "Return an object representing the repo USERNAME/REPO.
Defaults to the current repo.

The returned object is a decoded JSON object (plist)."
  (setq username (or username (githubv3/repo-owner)))
  (setq repo (or repo (githubv3/repo-name)))
  (remhash (cons username repo) githubv3/-repo-obj-cache)
  (githubv3/cached-repo-obj username repo))

(defun githubv3/cached-repo-obj (&optional username repo)
  "Return a (possibly cached) object representing the repo USERNAME/REPO.
Defaults to the current repo.

The returned object is a decoded JSON object (plist).

This differs from `githubv3/repo-obj' in that it returns a cached
copy of the repo object if one exists.  This is useful for
properties such as :parent and :fork that are highly unlikely to
change."
  (setq username (or username (githubv3/repo-owner)))
  (setq repo (or repo (githubv3/repo-name)))
  (let ((cached (gethash (cons username repo) githubv3/-repo-obj-cache)))
    (or cached
        (let* ((url-request-method "GET")
               (obj (plist-get
                     (githubv3/retrieve-synchronously
                      (list "repos" "show" username repo))
                     :repository)))
          (puthash (cons username repo) obj githubv3/-repo-obj-cache)
          obj))))


;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/repo-collaborators (&optional username repo)
  "Return an array of names of collaborators on USERNAME/REPO.
Defaults to the current repo."
  (setq username (or username (githubv3/repo-owner)))
  (setq repo (or repo (githubv3/repo-name)))
  (let ((url-request-method "GET"))
    (plist-get
     (githubv3/retrieve-synchronously
      (list "repos" "show" username repo "collaborators"))
     :collaborators)))

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/repo-network (&optional username repo)
  "Return an array of forks and/or parents of USERNAME/REPO.
Defaults to the current repo.

Each fork is a decoded JSON object (plist)."
  (setq username (or username (githubv3/repo-owner)))
  (setq repo (or repo (githubv3/repo-name)))
  (let ((url-request-method "GET"))
    (plist-get
     (githubv3/retrieve-synchronously
      (list "repos" "show" username repo "network"))
     :network)))

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/repo-parent-collaborators (&optional username repo)
  "Return an array of names of collaborators on the parent of USERNAME/REPO.
These are the default recipients of a pull request for this repo.
Defaults to the current repo.

If this repo has no parents, return the collaborators for it instead."
  (let ((parent (plist-get (githubv3/cached-repo-obj username repo) :parent)))
    (if (not parent) (githubv3/repo-collaborators username repo)
      (destructuring-bind (parent-owner . parent-repo) (githubv3/parse-repo parent)
        (githubv3/repo-collaborators parent-owner parent-repo)))))

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/untracked-forks ()
  "Return a list of forks of this repo that aren't being tracked as remotes.
Returned repos are decoded JSON objects (plists)."
  (lexical-let ((remotes (magit-git-lines "remote")))
    (delq "origin" remotes)
    (push (githubv3/repo-owner) remotes)
    (githubv3/-remove-if
     (lambda (repo) (member-ignore-case (plist-get repo :owner) remotes))
     (githubv3/repo-network))))


;;; Local Repo Information

(defun githubv3/repo-info ()
  "Return information about this GitHub repo.
This is of the form given by `githubv3/remote-info'.

Error out if this isn't a GitHub repo."
  (or (githubv3/remote-info "origin")
      (error "Not in a GitHub repo")))

(defun githubv3/repo-owner ()
  "Return the name of the owner of this GitHub repo.

Error out if this isn't a GitHub repo."
  (car (githubv3/repo-info)))

(defun githubv3/repo-name ()
  "Return the name of this GitHub repo.

Error out if this isn't a GitHub repo."
  (cadr (githubv3/repo-info)))

(defun githubv3/repo-ssh-p ()
  "Return non-nil if this GitHub repo is checked out via SSH.

Error out if this isn't a GitHub repo."
  (caddr (githubv3/repo-info)))


;;; Diff Information

(defun githubv3/section-index (section)
  "Return the index of SECTION as a child of its parent section."
  (githubv3/-position section (magit-section-children (magit-section-parent section))))

(defun githubv3/hunk-lines ()
  "Return the two line numbers for the current line (which should be in a hunk).
The first number is the line number in the original file, the
second is the line number in the new file.  They're returned
as (L1 L2).  If either doesn't exist, it will be nil.

If something goes wrong (e.g. we're not in a hunk or it's in an
unknown format), return nil."
  (block nil
    (let ((point (point)))
      (save-excursion
        (beginning-of-line)
        (when (looking-at "@@") ;; Annotations don't have line numbers,
          (forward-line)        ;; so we'll approximate with the next line.
          (setq point (point)))
        (goto-char (magit-section-beginning (magit-current-section)))
        (unless (looking-at "@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)") (return))
        (let ((l (- (string-to-number (match-string 1)) 1))
              (r (- (string-to-number (match-string 2)) 1)))
          (forward-line)
          (while (<= (point) point)
            (unless (looking-at "\\+") (incf l))
            (unless (looking-at "-") (incf r))
            (forward-line))
          (forward-line -1)
          (list (unless (looking-at "\\+") l) (unless (looking-at "-") r)))))))


;;; Network

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/track (username &optional repo fetch)
  "Track USERNAME/REPO as a remote.
If FETCH is non-nil, fetch that remote.

Interactively, prompts for the username and repo.  With a prefix
arg, fetches the remote."
  (interactive
   (destructuring-bind (username . repo) (githubv3/read-untracked-fork)
     (list username repo current-prefix-arg)))
  (magit-run-git "remote" "add" username (githubv3/repo-url username repo))
  (when fetch (magit-run-git-async "remote" "update" username))
  (message "Tracking %s/%s%s" username repo
           (if fetch ", fetching..." "")))

;;; Creating Repos

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/gist-repo (&optional private)
  "Upload the current repo as a Gist.
If PRIVATE is non-nil or with a prefix arg, the Gist is private.

Copies the URL of the Gist into the kill ring.  If
`githubv3/view-gist' is non-nil (the default), opens the gist in
the browser with `browse-url'."
  (interactive "P")
  (let ((url-max-redirections 0)
        (url-request-method "POST")
        (githubv3/api-base githubv3/gist-url)
        (githubv3/request-data
         `(,@(if private '(("private" . "1")))
           ("file_ext[gistfile1]" . ".dummy")
           ("file_name[gistfile1]" . "dummy")
           ("file_contents[gistfile1]" .
            "Dummy Gist created by GithubV3. To be replaced with a real repo.")))
        githubv3/parse-response)
    (let (url)
      (with-current-buffer (githubv3/retrieve-synchronously "gists")
        (goto-char (point-min))
        (re-search-forward "^Location: \\(.*\\)$")
        (setq url (match-string 1))
        (kill-buffer))
      (kill-new url)
      (let ((ssh-url (replace-regexp-in-string
                      "^http://gist\\.github\\.com/"
                      "git@gist.github.com:" url)))
        (magit-run-git "remote" "add" "origin" ssh-url)
        (magit-set "origin" "branch" "master" "remote")
        (magit-set "refs/heads/master" "branch" "master" "merge")
        (magit-run-git-async "push" "-v" "-f" "origin" "master")
        (when githubv3/view-gist (browse-url url))
        (message "Gist created: %s" url)))))

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/create-from-local (name &optional description homepage private)
  "Create a new GitHub repository for the current Git repository.
NAME is the name of the GitHub repository, DESCRIPTION describes
the repository, URL is the location of the homepage.  If PRIVATE
is non-nil, a private repo is created.

When called interactively, prompts for NAME, DESCRIPTION, and
HOMEPAGE.  NAME defaults to the name of the current Git
directory.  By default, creates a public repo; with a prefix arg,
creates a private repo."
  (interactive
   (list (read-string "Repository name: "
                      (file-name-nondirectory
                       (directory-file-name
                        (expand-file-name
                         (magit-get-top-dir default-directory)))))
         (read-string "Description: ")
         (read-string "Homepage: ")
         current-prefix-arg))
  
  (let ((url-request-method "POST")
        (githubv3/request-data `(("name" . ,name)
                                 ("description" . ,description)
                                 ("homepage" . ,homepage)
                                 ("private" . ,(if private "0" "1")))))
    (githubv3/retrieve "repos/create"
                       (lambda (data name)
                         (magit-git-string
                          "remote" "add" "origin"
                          (githubv3/repo-url (githubv3/config "user") name 'ssh))
                         (magit-set "origin" "branch" "master" "remote")
                         (magit-set "refs/heads/master" "branch" "master" "merge")
                         (magit-run-git-async "push" "-v" "origin" "master")
                         (message "GitHub repository created: %s"
                                  (plist-get (plist-get data :repository) :url)))
                       (list name))))

;;; Forking Repos

;;;
;;; NOT CONVERTED TO V3 YET
;;;

(defun githubv3/fork-current ()
  "Fork the current repository in place."
  (interactive)
  (destructuring-bind (owner repo _) (githubv3/repo-info)
    (let ((url-request-method "POST"))
      (githubv3/retrieve (list "repos" "fork" owner repo)
                         (lambda (obj repo buffer)
                           (with-current-buffer buffer
                             (magit-with-refresh
                               (magit-set (githubv3/repo-url
                                           (car (githubv3/auth-info))
                                           repo 'ssh)
                                          "remote" "origin" "url")))
                           (message "Forked %s/%s" owner repo))
                         (list repo (current-buffer))))))

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/send-pull-request (text recipients)
  "Send a pull request with text TEXT to RECIPIENTS.
RECIPIENTS should be a list of usernames."
  (let ((url-request-method "POST")
        (githubv3/request-data (cons (cons "message[body]" text)
                                     (mapcar (lambda (recipient)
                                               (cons "message[to][]" recipient))
                                             recipients)))
        (githubv3/api-base githubv3/github-url)
        (url-max-redirections 0) ;; GitHub will try to redirect, but we don't care
        githubv3/parse-response)
    (githubv3/retrieve (list (githubv3/repo-owner) (githubv3/repo-name)
                             "pull_request" (githubv3/name-rev-for-remote "HEAD" "origin"))
                       (lambda (_)
                         (kill-buffer)
                         (message "Your pull request was sent.")))))

;;;
;;; NOT CONVERTED TO V3 YET
;;;


(defun githubv3/pull-request (recipients)
  "Compose a pull request and send it to RECIPIENTS.
RECIPIENTS should be a list of usernames.

Interactively, reads RECIPIENTS via `githubv3/read-pull-request-recipients'.
For non-interactive pull requests, see `githubv3/send-pull-request'."
  (interactive (list (githubv3/read-pull-request-recipients)))
  (with-githubv3/message-mode
    (magit-log-edit-set-field
     'recipients (mapconcat 'identity recipients crm-separator)))
  (githubv3/pop-to-message "send pull request"))

(defun githubv3/toggle-ssh (&optional arg)
  "Toggle whether the current repo is checked out via SSH.
With ARG, use SSH if and only if ARG is positive."
  (interactive "P")
  (if (null arg) (setq arg (if (githubv3/repo-ssh-p) -1 1))
    (setq arg (prefix-numeric-value arg)))
  (magit-set (githubv3/repo-url (githubv3/repo-owner) (githubv3/repo-name) (> arg 0))
             "remote" "origin" "url")
  (magit-refresh-status))

(defvar githubv3/status-buffer nil
  "The Magit status buffer for the current buffer's Git repository.")
(make-variable-buffer-local 'githubv3/status-buffer)

(provide 'githubv3)

;;;###autoload
(eval-after-load 'magit
  '(unless (featurep 'githubv3)
     (require 'githubv3)))

;;; githubv3.el ends here
