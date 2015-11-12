
(when *is-a-windows* 
  (let (
        (mypaths
         '(
          "C:/Windows/system32/"
          "C:/Windows/"
           "E:/cygwin/usr/local/bin" 
           "E:/cygwin/usr/bin" 
           "E:/cygwin/bin" 
           "E:/android4.4/adt-bundle-windows-x86-20131030/sdk/platform-tools"
           ) )
        )

    (setenv "PATH" (mapconcat 'identity mypaths ";") )

    (setq exec-path (append mypaths (list "." exec-directory)) )
    ) )

(provide 'init-exec-path)
