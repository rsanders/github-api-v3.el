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

(provide 'github-api-v3-repos)
