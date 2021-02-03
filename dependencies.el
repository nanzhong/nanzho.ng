;;; dependencies.el --- Install and configure dependencies

;; Setup package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install and configure dependencies
;; (add-to-list 'load-path "../../emacs-love/weblorg")
;; (require 'weblorg)
(use-package weblorg :ensure t)

(use-package org
  :config
  (setq org-src-preserve-indentation t))

(use-package htmlize
  :ensure t
  :config
  (setq org-html-htmlize-output-type 'css))
(use-package rainbow-delimiters :ensure t)

(use-package python
  :config
  (setq python-indent-guess-indent-offset-verbose nil))

;;; dependencies.el ends here
