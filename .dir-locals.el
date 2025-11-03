((org-mode . ((eval . (progn
			
			(defun my-html-filter-nobreaks (text backend info)
			  "Remove newlines between CJK characters in HTML export."
			  (when (org-export-derived-backend-p backend 'html)
			    (replace-regexp-in-string 
			     "\\([\u3000-\u9fff\uff00-\uffef]\\)\n\\([\u3000-\u9fff\uff00-\uffef]\\)"
			     "\\1\\2"
			     text)))
			
			(unless (memq 'my-html-filter-nobreaks org-export-filter-plain-text-functions)
			  (add-to-list 'org-export-filter-plain-text-functions
				       'my-html-filter-nobreaks))

			(defun my-html-image-width (text backend info)
			  "Set all images to width 100% in HTML export."
			  (when (org-export-derived-backend-p backend 'html)
			    (replace-regexp-in-string 
			     "<img src=\"\\([^\"]+\\)\"" 
			     "<img style=\"width:100%\" src=\"\\1\"" 
			     text)))

			(unless (memq 'my-html-image-width org-export-filter-final-output-functions)
			  (add-to-list 'org-export-filter-final-output-functions
				       'my-html-image-width)))))))
