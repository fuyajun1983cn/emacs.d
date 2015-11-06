;;*********************************************;;
;;                                             ;;
;;**************Org模式相关配置*****************;;
;;                                             ;;
;;*********************************************;;
;; General Settings

;;(setq org-completion-use-ido t)
;;(setq org-outline-path-complete-in-steps nil)
;;(setq org-blank-before-new-entry t)
;; set maximum indention for decription lists
;;(setq org-list-description-max-indent 5)

;; prevent demoting heading also shifting text inside sections
;;(setq org-adapt-indentation nil)

;; Org-Agenda
(global-set-key "\C-ca" 'org-agenda)
(require 'org-agenda)

(setq org-directory "~/org")


;; Org-Capture
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      (quote (
	      ("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
	       "* TODO %?\n %i\n %a")
	      ("j" "Journal" entry (file+datetree "~/org/journal.org")
	       "* %?\nEntered on %U\n %i\n %a")
	      )))
(setq org-default-notes-file (concat org-directory "~/org/notes.org"))

;; Org-link
(global-set-key "\C-cl" 'org-store-link)
(add-hook 'org-load-hook
  (lambda ()
    (define-key org-mode-map "\C-n" 'org-next-link)
    (define-key org-mode-map "\C-p" 'org-previous-link)))


;;TAGS
(setq org-tag-alist (quote (("EMACS" . ?e)
			    ("WIFI" . ?w)
			    ("BUG" . ?b)
			    ("ME" . ?m)
			    ("STUDY" . ?s))))

;;图文混排模式
;;(iimage-mode 1)
(add-hook 'org-mode-hook 'org-toggle-inline-images)

;;TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED")
	(sequence "Submitted" "Assigned" "Working" "|" "Resolved")))
(setq org-log-done 'time)
(setq org-log-done 'note)


;; Some initial languages we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   ))


(provide 'init-org)
