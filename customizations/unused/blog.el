;;;; blog.el
;;;; settings for blogging
;;;; Leo Laporte, May 2022

;; Easy-Hugo https://github.com/masasam/emacs-easy-hugo
(use-package easy-hugo
  :init
  (setq easy-hugo-basedir "~/www/leofm/"
        easy-hugo-url "https://leo.fm"
        easy-hugo-sshdomain "leo.fm"
        easy-hugo-root "/home/leo/www/leofm/"
        easy-hugo-previewtime "300"
        easy-hugo-postdir "content/posts")
  :bind ("C-c C-e" . easy-hugo))

