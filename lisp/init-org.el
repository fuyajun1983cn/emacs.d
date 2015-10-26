;;*********************************************;;
;;                                             ;;
;;**************Org模式相关配置*****************;;
;;                                             ;;
;;*********************************************;;
(setq org-default-notes-file "~/.notes.org")
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;;图文混排模式
;;(iimage-mode 1)
(add-hook 'org-mode-hook 'org-toggle-inline-images)

;;TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
	(sequence "Submitted" "Assigned" "Working" "|" "Resolved")))
(setq org-log-done 'time)
(setq org-log-done 'note)

(provide 'init-org)
