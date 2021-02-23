(define cron
  (make <service>
    #:provides '(cron)
    #:docstring "Run `mcron'"
    #:start (make-forkexec-constructor
             `("mcron"
               "--daemon"
               ,(string-append (getenv "XDG_CONFIG_HOME")
                               "/cron/rclone.guile")))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(register-services cron)
(start cron)
