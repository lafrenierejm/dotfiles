(define cron
  (make <service>
    #:provides '(cron)
    #:requires '()
    #:start (make-system-constructor "mcron &")
    #:stop (make-system-destructor "pkill mcron")
    #:respawn? #t))

(register-services cron)
(start cron)
