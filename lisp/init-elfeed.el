;;
;; Elfeed: Web Feed Reader
;; https://github.com/skeeto/elfeed
;;
(require-package 'elfeed)
(maybe-require-package 'elfeed-org)

(require 'elfeed)

(global-set-key (kbd "C-x w") 'elfeed)

;; RSS that I'm interested
(setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "http://www.terminally-incoherent.com/blog/feed/"))

(setf url-queue-timeout 30)

;;search filter
(setq-default elfeed-search-filter "@2-months-ago +unread ")

;; Entries older than 2 weeks are marked as read
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                                            :remove 'unread))
