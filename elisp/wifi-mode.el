;;; wifi-mode.el --- wifi driver and wpa_supplicant  output syntax highlighting
;; COPYRIGHT © 2016, by Yajun Fu

;; Author: Yajun Fu (fuyajun1983cn@163.com)
;; Version: 0.0.1
;; Keywords: wifi log

;; This file is not part of GNU Emacs.

;;; License:


;;; Commentary:

;; wifi log syntax highlighting


;;; Code:

;;define sever category of keywords
(setq p2p-keywords '("CTRL-EVENT-EAP-STARTED"
;;P2P EVENTS
                     "P2P-DEVICE-FOUND"
                     "P2P-GO-NEG-SUCCESS"
                     "P2P-GROUP-FORMATION-SUCCESS"
                     "P2P-GROUP-FORMATION-FAILURE"
                     "P2P-GROUP-STARTED"
                     "P2P-GROUP-REMOVED"
                     "AP-STA-DISCONNECTED"
                     "CTRL-EVENT-DISCONNECTED"
                     "P2P-DEVICE-LOST"
                     "WPS-SUCCESS"
                     "P2P-INVITATION-RECEIVED"))
(setq category-keywords '("WifiStateMachine"
                          "WifiP2pService"
                          "wpa_supplicant"))

;;;; generate regex string for each category of keywords
(setq p2p-keywords-regexp  (regexp-opt p2p-keywords 'words ))
(setq category-keywords-regexp (regexp-opt category-keywords 'words))
;; create the list for font-lock.
;; each category of keyword is given a particular face
(defvar wifi-font-lock-keywords)
(setq wifi-font-lock-keywords `(
                                (,p2p-keywords-regexp . font-lock-keyword-face)
                                (,category-keywords-regexp . font-lock-variable-name-face)
                                  )
)

;;;###autoload
(define-derived-mode wifi-mode fundamental-mode
  "wifi mode"
  "Major mode for wifi output"

  (setq font-lock-defaults '((wifi-font-lock-keywords)))
)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wifi\\'" . wifi-mode))

;; add the mode to the `features' list
(provide 'wifi-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; wifi-mode.el ends here    
