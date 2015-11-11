;;============================================;;
;;                                                                                                         ;;
;;============= 基本配色方案====================;;
;;                                                                                                         ;;
;;============================================;;
(require-package 'color-theme)
(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)


(defun site-lisp-dir-for (name)
  (expand-file-name (format "site-lisp/%s" name) user-emacs-directory))

(add-to-list 'custom-theme-load-path (site-lisp-dir-for "airline-themes"))
(add-to-list 'custom-theme-load-path (site-lisp-dir-for "emacs-theme-gruvbox"))

;;new-style theme support, in which per-frame theming is not possible
;;------------------------------------------------------------------------------

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(gruvbox airline-dark))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme t)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))




;;--------------Default Theme------------------------------------
(require 'airline-themes)

;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-solarized-light))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-solarized-dark))
  (reapply-themes))

(defun default-theme()
  "Activate teh default color theme."
  (interactive)
  (setq custom-enabled-themes '(gruvbox airline-dark))
  (reapply-themes))

(add-hook 'after-init-hook 'default-theme)

(provide 'init-themes)
