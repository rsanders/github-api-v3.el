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

(defmacro when-ert-loaded (&rest body)
  `(dont-compile
     (when (featurep 'ert)
       ,@body)))

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

(defvar githubv3/auth-type "token"
  "Set to 'token' or 'basic' to determine auth type used")

;; output and caches

(defvar githubv3/users-history nil
  "A list of users selected via `githubv3/read-user'.")

(defvar githubv3/repos-history nil
  "A list of repos selected via `githubv3/read-repo'.")

(defvar githubv3/-repo-obj-cache (make-hash-table :test 'equal)
  "A hash from (USERNAME . REPONAME) to decoded JSON repo objects (plists).
This caches the result of `githubv3/repo-obj' and
`githubv3/cached-repo-obj'.")

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

(defun githubv3/request-data-as-json (&rest args)
  "Return the requests data for the in-process request as a JSON string

Takes a single string and return that, or an array and convert it to JSON"
  (let ((data (if (null args)
                  githubv3/request-data
                (car args))))
    (cond
     ((stringp data) data)
     ((null data)    "")
     (t              (json-encode data)))
  ))

;;; Requests

(defun githubv3/request-url (path)
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



(defmacro githubv3/with-auth (&rest body)
  `(if (equal githubv3/auth-type "basic")
       (githubv3/with-basic-auth
         ,@body)
     (githubv3/with-token-auth
      ,@body)))


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


;;;
;;; Not verified for v3
;;;
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
    (let ((url-mime-accept-string "application/json")         
          (url-request-data (githubv3/request-data-as-json)))
      (lexical-let ((callback callback) (githubv3/parse-response githubv3/parse-response))
        (url-retrieve (githubv3/request-url path)
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
    (let ((url-mime-accept-string "application/json")
          (url-request-data (githubv3/request-data-as-json)))
      (with-current-buffer (url-retrieve-synchronously (githubv3/request-url path))
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

;;;###autoload
(defun githubv3/config (key)
  "Returns a GitHub specific value from the global Git config."
  (magit-git-string "config" "--global" (concat "github." key)))


;;;###autoload
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
      (setq token (read-string (format "GitHub API token for '%s': " user)))
      (githubv3/set-config "v3token" token))
    
    (cons user token)))

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
      (setq password (read-string (format "GitHub password for '%s': " user))))
    
    (cons user password)))


;;; OAuth

;;;###autoload
(defun githubv3/create-auth-token ()
  "Create an authorization"
  (let ((url-request-method "POST")
        (githubv3/request-data '(("scopes" . ["user", "public_repo", "repo", "gist"])))
        (githubv3/auth-type "basic"))
    (githubv3/retrieve-synchronously
     (list "authorizations"))
    ))

;;;###autoload
(defun githubv3/list-auth-tokens ()
  "Create an authorization"
  (let ((url-request-method "GET")
        (githubv3/auth-type "basic"))
    (githubv3/retrieve-synchronously
     (list "authorizations"))
    ))

;;; Users

;;;###autoload
(defun githubv3/user-info (&optional user)
  "Get info about a given user.

If a username not supplied, the authenticated user will be used. "
  (let ((url-request-method "GET"))
    (githubv3/retrieve-synchronously
     (if user (list "users" user)
       (list "user"))
     )))

;;;###autoload
(defun githubv3/my-user-name ()
  "Return the authenticated user's name"
  (plist-get (githubv3/user-info) :login))

;;;###autoload
(defun githubv3/my-user-id ()
  "Return the authenticated user's ID"
  (plist-get (githubv3/user-info) :id))

(when-ert-loaded
 (ert-deftest ghv3-test-my-user-info ()
   "Tests fetching a user's information"
   (should (equal (githubv3/my-user-name) "robertzx"))
   (should (equal (githubv3/my-user-id) 292370)))
 
 (ert-deftest ghv3-test-auth-info ()
   "Tests fetching a user's token auth info"
   (should (equal (car (githubv3/auth-info)) "robertzx"))
   (should (equal (substring (cdr (githubv3/auth-info)) 0 5) "c3e1a"))))




;;; Repositories

;;;###autoload
(defun githubv3/repos-for-user (user)
  "Return an array of all repos owned by USER.
The repos are decoded JSON objects (plists)."
  (let ((url-request-method "GET"))
    (githubv3/retrieve-synchronously
     (list "users" user "repos"))))



;;; Gists

;;;###autoload
(defun githubv3/gists-for-user (user)
  "Return an array of all repos owned by USER.
The repos are decoded JSON objects (plists)."
  (let ((url-request-method "GET"))
    (githubv3/retrieve-synchronously
      (list "users" user "gists"))
     ))

;;;###autoload
(defun githubv3/get-gist (gist)
  "Return info about a named gist.
The result is a decoded JSON object (plists)."
  (let ((url-request-method "GET"))
    (githubv3/retrieve-synchronously
     (list "gists" gist))
    ))

;;;###autoload
(defun githubv3/fork-gist (gist)
  "Fork a named gist and return the URL."
  (let ((url-request-method "POST"))
    (githubv3/retrieve-synchronously
     (list "gists" gist "fork"))
    ))

;;
;; TODO: make this deal with the return value appropriately.
;;    if successful, gets an HTTP 204 back with no body.
;;
;;;###autoload
(defun githubv3/delete-gist (gist)
  "Delete a named gist and return the URL."
  (let ((url-request-method "DELETE")
        (githubv3/parse-response nil))
    (githubv3/retrieve-synchronously
     (list "gists" gist))
    ))



;;; Pull requests



;;; GitHub Information




(provide 'github-api-v3)

;;;###autoload
(eval-after-load 'magit
  '(unless (featurep 'github-api-v3)
     (require 'github-api-v3)))

;;; githubv3.el ends here
