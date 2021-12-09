;;; update.el --- Update package versions

;; Setup packages
(add-to-list 'load-path ".")
(load "init")

(straight-pull-all)
(straight-freeze-versions)

;;; update.el ends here
