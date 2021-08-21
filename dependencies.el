;;; dependencies.el --- Install and configure dependencies

(setq package-enable-at-startup nil)

;; Boostrap straight.el
;; Configure straight package version lockfile
(setq straight-profiles '((nil . "/workspace/straight-versions.el")))
;; Disable symlinks since that seems to cause problems with app platform
(setq straight-use-symlinks nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Bootstrap use-package
(straight-use-package 'use-package)

(use-package weblorg
  :straight t)

(use-package org
  :straight t
  :config
  (setq org-src-preserve-indentation t
        org-html-toplevel-hlevel 1))

(use-package htmlize
  :straight t
  :config
  (setq org-html-htmlize-output-type 'css))
(use-package rainbow-delimiters
  :straight t)

(use-package python
  :straight t
  :config
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package fish-mode
  :straight t)

;; (straight-pull-all)
;; (straight-freeze-versions)
;; (straight-thaw-versions)

;;; dependencies.el ends here
