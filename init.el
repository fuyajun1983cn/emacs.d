;;; init.el --- Where all the magic begins
;;
;; This is the first script to get loaded.
;;

;;
;;  (setq debug-on-err t) when debugging
;;

;; load Org-mode from source when the ORG_HOME environment variable is set

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;
;; Byte compile
;; emacs -batch -f batch-byte-compilefile.el.

(package-initialize)
(setenv "ORG_HOME" "/home/jackson/Desktop/org-8.3.4/lisp")
(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (require 'org))))

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq fyj-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (getenv "ORG_HOME")
                  (file-directory-p (expand-file-name "lisp"
                                                      (getenv "ORG_HOME"))))
       '(require 'org))
    ;; load up the starter kit
    (org-babel-load-file (expand-file-name "fyj.org" fyj-dir))))

;;; init.el ends here
