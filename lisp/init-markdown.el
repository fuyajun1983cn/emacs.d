(require-package 'markdown-mode)

;;
;;Don't enable whitespace-cleanup-mode in markdown-mode
;;
(after-load 'whitespace-cleanup-mode
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))


(provide 'init-markdown)
