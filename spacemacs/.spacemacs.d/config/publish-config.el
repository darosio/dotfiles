(require 'ox-publish)

(setq org-html-html5-fancy t
      org-html-htmlize-output-type 'css
      org-html-head-include-default-style t
      )

(setq org-publish-project-alist
      `(("metaModels-notes"
         ;; Publish details
         :base-directory "~/metamodels/"
         :exclude-tags ("noexport" "abstract")
         :base-extension "org"
         :recursive t
         ;; :exclude (regexp-opt "themes" "cv" "README.org")
         :publishing-function org-html-publish-to-html
         ;; :html-postamble pank.eu-postamble
         :publishing-directory "~/public"
         :html-head-extra "<link rel="stylesheet" type="text/css" href="http://thomasf.github.io/solarized-css/solarized-light.min.css" />"
         ;; Org fiddling
         ;; :html-head-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"./css/style.css\">"
         ;; html fiddling
         ;; :html-preamble (pank.eu-preamble nil "PhD candidate in economics")
         ;; :sitemap-filename "index.org"
         :table-of-contents t
         :toc 3)

        ("metaModels-static"
         :base-directory "~/metamodels/"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "~/public"
         :recursive t
         :publishing-function org-publish-attachment)

        ("css"
         :base-directory "~/metamodels/css/"
         :base-extension "css"
         :recursive t
         :publishing-directory "~/public/css/"
         :publishing-function org-publish-attachment)

        ("metaModels"
         :components ("metaModels-notes" "metaModels-static" "css")
         :auto-sitemap t
         ;; #+INFOJS_OPT: view:t toc:t ltoc:t mouse:underline buttons:0 path:http://thomasf.github.io/solarized-css/org-info.min.js
         ;; :infojs_opt "view:t toc:t ltoc:t mouse:underline buttons:0 path:http://thomasf.github.io/solarized-css/org-info.min.js"
         ))
      )

(provide 'publish-config)
:html-head-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"./myfile.css\">"
