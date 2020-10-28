(define transmission
  (make <service>
    #:provides '(transmission)
    #:doctring "Run `transmission-daemon'"
    #:start (make-forkexec-constructor
             `("transmission-daemon"
               "--log-error"
               "--logfile" ,(string-append
                             (getenv "HOME")
                             "/.local/share/transmission/log.txt")
               "--download-dir" ,(string-append (getenv "HOME") "/tmp")))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(register-services transmission)
(start transmission)
