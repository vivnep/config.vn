(use-package org
  :config
  ;; encountered this https://raindev.io/blog/how-to-not-write-emacs-config-in-org/
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
  )
