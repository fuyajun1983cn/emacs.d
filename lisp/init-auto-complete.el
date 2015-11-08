;;*********************************************;;
;;                                             ;;
;;**************auto-complete模式相关配置*******;;
;;                                             ;;
;;*********************************************;;
(require-package 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/mode/auto-complete/dict")
(define-key ac-mode-map (kbd "M-/") 'auto-complete);;auto-complete command
;;trigger auto-complete
;(ac-set-trigger-key "TAB")
(setq ac-auto-start 3);当播入3个字符的时候，开始自动补全
;;complete menu color
(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")
;My Dictionary
;;(add-to-list 'ac-user-dictionary-files "~/.emacs.d/mydict")

(provide 'init-auto-complete)
