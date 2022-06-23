;;;; Org-mode Customizations
;;;; Leo Laporte, April 2022

;; make sure the latest org is installed before anything calls it
(straight-use-package 'org)

;; load org-roam
(use-package org-roam
  :straight t
  :ensure t
  :init
  ;; Help keep the `org-roam-buffer', toggled via `org-roam-buffer-toggle', sticky.
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\#"
		 (display-buffer-in-side-window)
		 (side . right)
		 (slot . 0)
		 (window-width . 0.33)
		 (window-parameters . ((no-other-window . t)
				       (no-delete-other-windows . t)))))
  ;; When t the autocomplete in org documents will query the org roam database
  (setq org-roam-completion-everywhere t)
  (setq org-roam-v2-ack t)
  (org-roam-db-autosync-mode)

  :config
  (setq org-roam-dailies-capture-templates
	'(("i" "item" item
	   "[ ] %?"
	   :target (file+head "%<%Y-%m-%d>.org"
			      "#+title: %<%Y-%m-%d>\n#+FILETAGS: :dailies:\n"))))
  (setq org-tag-alist '(("@abby" . ?a)
			("@family" . ?f)
			("@finance" . ?$)
			("@health" . ?e)
			("@home" . ?h)
			("@lisa" . ?l)
			("@personal" . ?p)
			("@travel" . ?t)
			("@work" . ?w)))

  :custom
  (org-roam-directory (file-truename "~/org/"))

  ;; See https://github.com/nobiot/org-transclusion/issues/136
  (org-roam-db-extra-links-exclude-keys '((node-property "ROAM_REFS")))
  (org-roam-node-display-template
   (concat "${type:7} "
	   " ${title:80} "
	   (propertize "${tags:50}" 'face 'org-tag)))
  (org-roam-node-annotation-function
   (lambda (node)
     (org-roam-node-backlinkscount node)))


  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ;; Dailies
	 ("C-c n j" . org-roam-dailies-capture-today)
	 :map org-mode-map
	 ("C-M-i"   . completion-at-point)
	 ("C-s-<right>" . org-roam-dailies-goto-next-note)
	 ("C-s-<left>" . org-roam-dailies-goto-previous-note)))

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
