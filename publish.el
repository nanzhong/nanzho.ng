;;; publish.el --- Generate static site using weblorg.el

;; Load dependencies
(add-to-list 'load-path ".")
(load "dependencies")

;; Set site wide configuration
(setq weblorg-default-url (let ((base-url (getenv "BASE_URL")))
                            (if (string= base-url "") "http://localhost:8080" base-url)))
(message weblorg-default-url)
(let ((site (weblorg-site
             :base-url weblorg-default-url
             :template-vars '(("title" . "nanzho.ng")
                              ("name" . "Nan Zhong")
                              ("menu" . ((("name" . "nanzho.ng")
                                          ("url" . "/"))
                                         (("name" . "about")
                                          ("url" . "/about/"))
                                         (("name" . "posts")
                                          ("url" . "/posts/"))))
                              ("projects" . (("personal" . ((("name" . "nanzhong/workstation")
                                                             ("desc" . "My declarative development environment")
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
                              ("gtm_container" . "GTM-WQ2MB8X")
                              ("disqus_shortname" . "nanzhong")))))
  (weblorg-route
   :name "index"
   :input-pattern "posts/**/*.org"
   :input-aggregate (lambda (posts)
                      (let* ((agg-posts (weblorg-input-aggregate-all-desc posts))
                             (posts (cdr (assoc "posts" (car agg-posts)))))
                        `((("posts" . ,(butlast posts (- (length posts) 5)))))))
   :template "index.html"
   :output "output/index.html"
   :url "/")
  (weblorg-route
   :name "pages"
   :input-pattern "pages/*.org"
   :template "page.html"
   :output "output/{{ slug }}/index.html"
   :url "/{{ slug }}/"
   :site site)
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
   :url "/posts")

  (setq debug-on-error t)
  (weblorg-export))

;;; publish.el ends here
