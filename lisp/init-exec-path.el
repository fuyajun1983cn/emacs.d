
(when *is-a-windows* 
  (let (
        (mypaths
         '(
          "C:/Windows/system32/"
          "C:/Windows/"
           "E:/cygwin/usr/local/bin" 
           "E:/cygwin/usr/bin" 
           "E:/cygwin/bin" 
           ) )
        )

    (setenv "PATH" (mapconcat 'identity mypaths ";") )

    (setq exec-path (append mypaths (list "." exec-directory)) )
    ) )

(provide 'init-exec-path)
