;;;; blog.el
;;;; settings for blogging
;;;; Leo Laporte, May 2022

;; Easy-Hugo https://github.com/masasam/emacs-easy-hugo
(straight-use-package 'easy-hugo)
(require 'easy-hugo)
:init
(setq easy-hugo-basedir "~/www/leofm/")
(setq easy-hugo-url "https://leo.fm")
(setq easy-hugo-sshdomain "leo.fm")
(setq easy-hugo-root "/home/leo/www/leofm/")
(setq easy-hugo-previewtime "300")
(setq easy-hugo-postdir "content/posts")
:bind
(define-key global-map (kbd "C-c C-e") 'easy-hugo)

