(setq make-backup-files nil)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode 1)
(delete-selection-mode 1)
(menu-bar-mode -1)
(global-hl-line-mode 1)
(set-face-background 'hl-line "gray13")
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

;; 支持emacs和外部程序的粘贴
(setq x-select-enable-clipboard t)

;; use xsel to copy/paste in emacs-nox
(unless window-system
  (when (getenv "DISPLAY")
    (defun xsel-cut-function (text &optional push)
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    (defun xsel-paste-function()
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output )))
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)))
