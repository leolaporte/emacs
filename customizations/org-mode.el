;;;; Org-mode Customizations
;;;; Leo Laporte, April 2022

;; make sure the latest org is installed before anything calls it
(straight-use-package 'org)

(setq org-tag-alist '(("@abby" . ?a)
		      ("@family" . ?f)
		      ("@finance" . ?$)
		      ("@health" . ?e)
		      ("@home" . ?h)
		      ("@lisa" . ?l)
		      ("@personal" . ?p)
		      ("@travel" . ?t)
		      ("@work" . ?w)))

;; load org-roam
(use-package org-roam
  :straight t
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
	 :map org-mode-map
	 ("C-M-i"   . completion-at-point)))

;; deft for search
(use-package deft
  :straight t
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

;; If you're using a vertical completion framework, you might want a more informative completion interface
(setq org-roam-node-display-template
      (concat "${title:*} "
	      (propertize "${tags:10}" 'face 'org-tag)))

(setq org-roam-completion-everythere t)
(org-roam-db-autosync-mode)

(require 'org-roam-protocol)
(require 'org-roam-export)

(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
	    #'org-roam-reflinks-section
	    ;; #'org-roam-unlinked-references-section
	    ))

;; Org-roam does not control how the pop-up buffer is displayed: this is left to the user. The author’s recommended configuration is as follows:
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
