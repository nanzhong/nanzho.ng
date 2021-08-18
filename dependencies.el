;;; dependencies.el --- Install and configure dependencies

(setq package-enable-at-startup nil)

;; Boostrap straight.el
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

;; Configure straight package version lockfile
(setq straight-profiles '((nil . "/workspace/straight-versions.el")))

;; Bootstrap use-package
(straight-use-package 'use-package)

(use-package weblorg
  :straight t)

(use-package org
  :straight t
  :config
  (setq org-src-preserve-indentation t))

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
