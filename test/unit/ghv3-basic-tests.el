
(require 'ert)

(when-ert-loaded
 (ert-deftest ghv3-test-my-user-info ()
  "Tests fetching a user's information"
  (should (equal (githubv3/my-user-name) "robertzx"))
  (should (equal (githubv3/my-user-id) 292370)))
 
 (ert-deftest ghv3-test-auth-info ()
   "Tests fetching a user's token auth info"
   (should (equal (car (githubv3/auth-info)) "robertzx"))
   (should (equal (substring (cdr (githubv3/auth-info)) 0 5) "c3e1a"))))

