;;*********************************************;;
;;                                             ;;
;;**************Org≈‰÷√Œƒº˛*******************;;
;;                                             ;;
;;*********************************************;;
;; General Settings

(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "someday.org"))

;;(org-export-html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">")
(setq org-fast-tag-selection-single-key nil)
(setq org-refile-targets (quote (("~/org/gtd.org":maxlevel . 1) ("~/org/someday.org":level . 2))))
(setq org-reverse-note-order nil)
(setq org-tags-column -78)
(setq org-tags-match-list-sublevels nil)
(setq org-time-stamp-rounding-minutes 5)
(setq org-use-fast-todo-selection t)
(setq org-use-tag-inheritance nil)
(setq org-deadline-warning-days 7)
(setq org-insert-mode-line-in-empty-file t)
;; Org-Agenda
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-files (quote ("~/org/gtd.org" "~/org/personal.org")))
(setq org-agenda-ndays 7)
(setq org-agenda-repeating-timestamp-show-all nil)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-sorting-strategy (quote ((agenda time-up priority-down tag-up) (todo tag-up))))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-todo-ignore-deadlines t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-window-setup (quote other-window))
(setq org-agenda-include-diary nil)
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; org-remober mode
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)

(setq org-remember-templates
     '(
      ("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" "gtd.org" "Tasks")
      ("Journal" ?j "\n* %^{topic} %T \n%i%?\n" "journal.org")
      ("Contact" ?c "\n* %^{Name} :CONTACT:\n%[contemp.txt]\n" "personal.org")
      ))

(setq org-agenda-custom-commands
'(

("P" "Projects"   
((tags "PROJECT")))

("H" "Office and Home Lists"
     ((agenda)
          (tags-todo "OFFICE")
          (tags-todo "HOME")
          (tags-todo "COMPUTER")
          (tags-todo "DVD")
          (tags-todo "READING")))

("D" "Daily Action List"
     (
          (agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)
                      ))))
)
)

(defun gtd ()
    (interactive)
    (find-file  (concat org-directory "gtd.org"))
)
(global-set-key (kbd "C-c g") 'gtd)


;; Org-Capture
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      (quote (
	      ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks") ;;Things That I need to do. 
	       "* TODO %^{Brief Description}  %^g\n%?\nAdded: %U")
	      ("j" "Journal" entry (file+datetree "~/org/journal.org")  ;; Things That I have done
	       "* %?\nEntered on %U\n %i\n %a")
	      )))

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

;;(iimage-mode 1)
(add-hook 'org-mode-hook 'org-toggle-inline-images)

;;auto fill mode
(add-hook 'org-mode-hook 'auto-fill-mode)

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
