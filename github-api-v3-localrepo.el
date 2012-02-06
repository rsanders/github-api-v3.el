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


;;; Local Repo Information - requires that the current buffer be "in"
;;; a local repo

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

(defun githubv3/toggle-ssh (&optional arg)
  "Toggle whether the current repo is checked out via SSH.
With ARG, use SSH if and only if ARG is positive."
  (interactive "P")
  (if (null arg) (setq arg (if (githubv3/repo-ssh-p) -1 1))
    (setq arg (prefix-numeric-value arg)))
  (magit-set (githubv3/repo-url (githubv3/repo-owner) (githubv3/repo-name) (> arg 0))
             "remote" "origin" "url")
  (magit-refresh-status))

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


(provide 'github-api-v3-localrepo)

