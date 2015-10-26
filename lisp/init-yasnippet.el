;; loading yasnippet will slow the startup
;; but it's necessary cost
(require 'yasnippet)

;; my private snippets, should be placed before enabling yasnippet
(setq my-yasnippets (expand-file-name "~/.emacs.d/misc/yasnippets"))
(if (and  (file-exists-p my-yasnippets) (not (member my-yasnippets yas-snippet-dirs)))
    (add-to-list 'yas-snippet-dirs my-yasnippets))

(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt yas-x-prompt yas-dropdown-prompt yas-completing-prompt))

(provide 'init-yasnippet)
