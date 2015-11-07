;;============================================;;
;;                                                                                                         ;;
;;============= 基本配色方案====================;;
;;                                                                                                         ;;
;;============================================;;
(defun site-lisp-dir-for (name)
  (expand-file-name (format "site-lisp/%s" name) user-emacs-directory))

(add-to-list 'custom-theme-load-path (site-lisp-dir-for "airline-themes"))
(add-to-list 'custom-theme-load-path (site-lisp-dir-for "emacs-theme-gruvbox"))

(require 'airline-themes)

(load-theme 'gruvbox t)
(load-theme 'airline-dark t)

(provide 'init-themes)
