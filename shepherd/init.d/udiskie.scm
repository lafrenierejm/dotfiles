(define auto-mount
  (make <service>
    #:provides '(auto-mount)
    #:requires '()
    #:start (make-system-constructor "udiskie &")
    #:stop (make-system-destructor "pkill udiskie")
    #:respawn? #t))

(register-services auto-mount)
(start auto-mount)
