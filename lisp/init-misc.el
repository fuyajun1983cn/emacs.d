;;----------------------------------------------------------------------------
;; Misc config - 与模式无关的一些通用配置
;;----------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)
;; ask me when I quit emacs
(set-variable 'confirm-kill-emacs 'yes-or-no-p)

(setq-default regex-tool-backend 'perl)

;;设置打开文件的缺省路径
(setq default-directory "~/")

;;设定不产生备份文件
(setq make-backup-files nil)
(setq-default make-backup-files nil)

;;鼠标中键可以粘贴
(setq mouse-yank-at-point t)

;;设置个人信息
(setq user-full-name "Fu Yajun")
(setq user-mail-address "fuyajun1983cn@163.com")

;;自动在文件末增加一行
(setq require-final-newline t)

;;当光标在行尾上下移动的时候，始终保持在行尾
(setq track-eol t) 

;;新行自动缩进
(define-key global-map (kbd "RET") 'newline-and-indent)

;;
;;放大字体: Ctrl-x Ctrl-+ 或 Ctrl-x Ctrl-=
;;缩小字体: Ctrl-x Ctrl–
;;重置字体: Ctrl-x Ctrl-0
(if *is-a-windows*
    (progn
        ;; For Windows
       (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
       (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease))
(progn
  ;; For Linux
  (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
  (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)))

;;
;; 自动插入文件头
;;
(auto-insert-mode)  ;;; Adds hook to find-files-hook
(setq auto-insert-directory "~/.emacs.d/misc/templates/") ;;; Or use custom, *NOTE* Trailing slash important
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion

(setq auto-insert-alist
      (append '((org-mode . "Template.org")
            (python-mode . "Template.py")
            (c-mode . "Template.c")
            )
           auto-insert-alist))

(global-set-key "\C-x\C-r" 'prefix-region)
(global-set-key "\C-x\C-l" 'goto-line)
(global-set-key "\C-x\C-y" 'copy-region-as-kill)

;; restore opened files, save cursor position, save minibuffer history
(desktop-save-mode 1)

(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saveplace.el") ) ; use standard emacs dir
(setq-default save-place t)

;; Save minibuffer history
(savehist-mode 1)

(defalias 'list-buffers 'ibuffer) ; make ibuffer default

;always show line numbers
(global-linum-mode 1)

;hilight current line globally
(global-hl-line-mode 1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)


(provide 'init-misc)
