;;; init.el --- Initialize emacs environment

(setq user-emacs-directory (expand-file-name "."))

;; Boostrap straight.el
;; Disable symlinks since that seems to cause problems with app platform.
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

;; Bootstrap use-package.
(straight-use-package 'use-package)

(use-package emacs
  :config
  ;; Silence indentation messages from sh-set-shell in sh-mode.
  (require 'cl-lib)
  (advice-add 'sh-set-shell :around
              (lambda (orig-fun &rest args)
                (cl-letf (((symbol-function 'message) #'ignore))
                  (apply orig-fun args)))))

(use-package org
  :straight t
  :config
  (setq org-src-preserve-indentation t
        org-html-toplevel-hlevel 1
        org-export-with-section-numbers nil
        org-id-locations-file (expand-file-name "./org-id-locations")))

(use-package org-roam
  :straight t
  :after org
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory (expand-file-name "./org")
        org-roam-dailies-directory "daily/"
        org-roam-file-extensions '("org")
        org-roam-db-location (expand-file-name "./org-roam.db")))

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

(use-package templatel
  :straight t)

(use-package weblorg
  :straight (:host github :repo "nanzhong/weblorg" :branch "org-roam"))

(use-package weblorg-org-roam)
;;; init.el ends here
