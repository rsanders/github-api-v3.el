github-api-v3.el -- Emacs library for the v3 GitHub API
=======================================================

This library provides an interface to the GitHub v3 API.

WARNING: as of 2012-02-05, development is still in early stages.

Install
-------

You can install with el-get or the following manual technique:

    $ cd ~/.emacs.d/vendor
    $ git clone git://github.com/rsanders/github-api-v3.el.git

In your emacs config:

    (add-to-list 'load-path "~/.emacs.d/vendor/github-api-v3.el)
    (require 'github-api-v3)

Usage
-----

This library does not provide any functions meant for end-users.


Authentication
==============

Most of the functions provided here use OAuth2 token
authentication. It is best to provide the library with a token in the
git config like so:

    git config --global github.user your_user_name
    git config --global github.v3token 37163872163826382716321

You can also configure the v3token per-project.

Two functions related to the creation and manipulation of OAuth2
tokens (listed below) require the user's username and password
credentials. The password is not saved anywhere outside of Emacs.  

Functions
=========

Authentication
--------------

* githubv3/create-auth-token - create an OAuth2 token suitable for use by this library. Requires basic auth, and thus a username and password.
* githubv3/list-auth-tokens - list all known OAuth2 tokens for the authenticated user; requires basic auth.
* githubv3/user-info - list information about a names user, or if no name supplied, the authenticated user.

Gists
-----

* githubv3/gists-for-user - return all the gists belonging to a user
* githubv3/get-gist - get a plist describing a given gist
* githubv3/fork-gist - fork a gist and return the new gist info
* githubv3/delete-gist - delete a gist by id

Repositories
------------

* githubv3/repos-for-user - return a plist for all the repos belonging to a given user

Requirements
============

Emacs 23 or above
Magit, JSON, url, and crm packages

Meta
====

* Code: `git clone git://github.com/rsanders/github-api-v3.el.git`
* Home: <http://github.com/rsanders/github-api-v3.el>
* Bugs: <http://github.com/rsanders/github-api-v3.el/issues>


