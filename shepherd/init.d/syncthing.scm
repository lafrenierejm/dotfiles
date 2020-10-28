(define syncthing
  (make <service>
    #:provides '(syncthing)
    #:doctring "Run `syncthing' without opening a web browser"
    #:start (make-forkexec-constructor
             `("syncthing"
               "-no-browser"
               "-logflags=3"            ; prefix with date and time
               ,(string-append
                 "--logfile="
                 (getenv "HOME")
                 "/.local/share/syncthing/log.txt")))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(register-services syncthing)
(start syncthing)
