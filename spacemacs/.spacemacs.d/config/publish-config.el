(require 'ox-publish)

(setq org-html-html5-fancy t
      ;; org-html-htmlize-output-type 'css
      org-html-head-include-default-style t
      )

(setq org-publish-project-alist
      `(("metaModels-notes"
         ;; Publish details
         :base-directory "~/workspace/arte/Web/metamodels/"
         :exclude-tags ("noexport" "abstract")
         :base-extension "org"
         :recursive t
         ;; :exclude (regexp-opt "themes" "cv" "README.org")
         :publishing-function org-html-publish-to-html
         ;; :html-postamble pank.eu-postamble
         :publishing-directory "~/public"
         ;; :html-head-extra "<link rel="stylesheet" type="text/css" href="http://thomasf.github.io/solarized-css/solarized-light.min.css">"
         ;; :html-head-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"./myfile.css\">"
         ;; Org fiddling
         ;; :html-head-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"./css/style.css\">"
         ;; html fiddling
         ;; :html-preamble (pank.eu-preamble nil "PhD candidate in economics")
         ;; :sitemap-filename "index.org"
         :headline-levels 4             ;; Just the default for this project.
         ;; :table-of-contents t
         ;; :toc 3
         :auto-preamble t
         )

         ("metaModels-static"
          :base-directory "~/workspace/arte/Web/metamodels/"
          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
          :publishing-directory "~/public"
          :recursive t
          :publishing-function org-publish-attachment)

        ("metaModels"
         :components ("metaModels-notes" "metaModels-static")
         :auto-sitemap t
         ;; #+INFOJS_OPT: view:t toc:t ltoc:t mouse:underline buttons:0 path:http://thomasf.github.io/solarized-css/org-info.min.js
         ;; :infojs_opt "view:t toc:t ltoc:t mouse:underline buttons:0 path:http://thomasf.github.io/solarized-css/org-info.min.js"
         ))
      )

(provide 'publish-config)
