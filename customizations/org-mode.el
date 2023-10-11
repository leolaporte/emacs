;;;; Org-mode Customizations
;;;; Leo Laporte, April 2022

;; make sure the latest org is installed before anything calls it
(use-package org)

;; load org-roam
(use-package org-roam
  :ensure t
  :init
  ;; Help keep the `org-roam-buffer', toggled via `org-roam-buffer-toggle', sticky.
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\#"
		 (display-buffer-in-side-window)
		 (side . right)
		 (slot . 0)
		 (window-width . 0.50)
		 (window-parameters . ((no-other-window . t)
				       (no-delete-other-windows . t)))))

  ;; When t the autocomplete in org documents will query the org roam database
  (setq org-roam-completion-everywhere t)
  (setq org-roam-v2-ack t)
  ;;  (setq find-file-visit-truename t)
  (org-roam-db-autosync-mode)

  :config
  (setq org-roam-dailies-directory "daily/")

  (setq org-roam-dailies-capture-templates
	'(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+FILETAGS: :dailies:\n"))))

  (setq org-tag-alist '(("@abby" . ?a)
			("@books" . ?b)
			("@family" . ?f)
			("@finance" . ?$)
			("@games" . ?g)
			("@health" . ?e)
			("@home" . ?h)
			("@lisa" . ?l)
			("@personal" . ?p)
			("@travel" . ?t)
			("@work" . ?w)))

  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ;; ("C-c n f" . org-roam-node-find)
	 ;; ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ;; Dailies
	 ("C-c n j" . org-roam-dailies-capture-today)
	 :map org-mode-map
	 ("C-M-i"   . completion-at-point)
	 ("C-s-<right>" . org-roam-dailies-goto-next-note)
	 ("C-s-<left>" . org-roam-dailies-goto-previous-note))

  :custom
  (setq org-roam-directory (file-truename "~/org-roam/"))
  (setq org-roam-verbose t)
  (setq org-roam-db-location (concat org-roam-directory "/.database/org-roam.db")))

;; deft for search
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(require 'org-roam-protocol)
(require 'org-roam-export)

(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
	    #'org-roam-reflinks-section
	    #'org-roam-unlinked-references-section))
