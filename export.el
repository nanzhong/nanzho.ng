;;; export.el --- Generate static site using weblorg.el

;; Setup packages
(add-to-list 'load-path ".")
(load "init")

;; Set site wide configuration
(setq weblorg-default-url (let ((base-url (getenv "BASE_URL")))
                            (if (string= base-url "") "http://localhost:8080" base-url)))

(let* ((site (weblorg-site
              :base-url weblorg-default-url
              :default-route "org-nodes"
              :theme nil
              :template-vars '(("title" . "nanzho.ng")
                               ("name" . "Nan Zhong")
                               ("menu" . ((("name" . "nanzho.ng")
                                           ("url" . "/"))
                                          (("name" . "org")
                                           ("url" . "/org/"))
                                          (("name" . "posts")
                                           ("url" . "/posts/"))))
                               ("projects" . (("personal" . ((("name" . "nanzhong/env")
                                                              ("desc" . "My declarative environments")
                                                              ("url" . "https://github.com/nanzhong/workstation"))
                                                             (("name" . "nanzhong/tester")
                                                              ("desc" . "Test runner and reporting framework")
                                                              ("url" . "https://github.com/nanzhong/tester"))
                                                             (("name" . "nanzhong/oktaauth")
                                                              ("desc" . "net/http compatible drop in Okta auth handler")
                                                              ("url" . "https://github.com/nanzhong/oktaauth"))))
                                              ("professional" . ((("name" . "DigitalOcean App Platform")
                                                                  ("desc" . "Opinionated PaaS")
                                                                  ("url". "https://www.digitalocean.com/docs/app-platform/"))
                                                                 (("name" . "DigitalOcean Kubernetes Service")
                                                                  ("desc" . "Managed kubernetes product")
                                                                  ("url" . "https://www.digitalocean.com/docs/kubernetes/"))))))
                               ;; Social
                               ("twitter" . "nanzhong")
                               ("github" . "nanzhong")
                               ("email" . "me@nanzho.ng")
                               ;; Integrations
                               ("gtm_container" . "GTM-KWMNL2X"))))
       (org-roam-nodes-filter (lambda (node)
                                (member "publish" (org-roam-node-tags node)))))
  ;; Temporarily disabled until I have an actual "page"...
  ;; (weblorg-route
  ;;  :name "pages"
  ;;  :input-pattern "pages/*.org"
  ;;  :template "page.html"
  ;;  :output "output/{{ slug }}/index.html"
  ;;  :url "/{{ slug }}/"
  ;;  :site site)
  (weblorg-route
   :name "posts"
   :input-pattern "posts/**/*.org"
   :template "post.html"
   :output "output/posts/{{ slug }}/index.html"
   :url "/posts/{{ slug }}/"
   :site site)
  (weblorg-route
   :name "posts-list"
   :input-pattern "posts/**/*.org"
   :input-aggregate #'weblorg-input-aggregate-all-desc
   :template "posts.html"
   :output "output/posts/index.html"
   :url "/posts"
   :site site)
  (weblorg-route
   :name "org"
   :input-pattern "org.org"
   :template-vars (let* ((org-nodes (cdr (assoc "nodes" (car (weblorg-input-source-org-roam-nodes-agg
                                                              org-roam-nodes-filter))))))
                    `(("nodes" . ,org-nodes)))
   :template "org.html"
   :output "output/org/index.html"
   :url "/org"
   :site site)
  (weblorg-route
   :name "org-nodes"
   :input-source (lambda () (weblorg-input-source-org-roam-nodes org-roam-nodes-filter))
   :template "org-node.html"
   :output "output/org/{{ slug }}/index.html"
   :url "/org/{{ slug }}/"
   :site site)
  (weblorg-route
   :name "index"
   :input-pattern "index.org"
   :template-vars (let* ((posts (weblorg--route-posts (weblorg--site-route site "posts-list")))
                         (posts (cdr (assoc "posts" (car posts))))
                         (org-nodes (cdr (assoc "nodes" (car (weblorg-input-source-org-roam-nodes-agg
                                                              org-roam-nodes-filter
                                                              (lambda (a b)
                                                                (time-less-p (org-roam-node-file-mtime b)
                                                                             (org-roam-node-file-mtime a)))
                                                              5))))))
                    `(("posts" . ,(butlast posts (- (length posts) 5)))
                      ("nodes" . ,org-nodes)))
   :template "index.html"
   :output "output/index.html"
   :url "/"
   :site site)
  (weblorg-copy-static
   :output "output/assets/{{ file }}"
   :url "/assets/{{ file }}")

  (setq debug-on-error t)
  (weblorg-export))

;;; export.el ends here
