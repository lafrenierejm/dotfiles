;; To apply:
;; sudo -E guix system -L ~/.config/guix/system reconfigure $path_to_this_file

(use-modules
 (gnu)
 (gnu system locale)
 (gnu system nss)
 (gnu system pam)
 (guix store)
 (guix gexp)
 (nongnu packages linux)
 (nongnu system linux-initrd)
 (rnrs lists)
 (srfi srfi-1))
(use-service-modules
 admin
 cups
 desktop
 linux
 mcron
 networking
 security-token
 ssh
 virtualization
 xorg)
(use-package-modules
 android
 ccache
 certs
 cups
 emacs-xyz
 ghostscript
 gnupg
 linux
 ntp
 scanner
 xorg
 virtualization
 vpn
 wm)

(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (host-name "earthbound")
  (timezone "America/Chicago")
  (locale "en_US.utf8")

  ;; Choose US English keyboard layout.  The "altgr-intl"
  ;; variant provides dead keys for accented characters.
  (keyboard-layout (keyboard-layout "us"))

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (target "/boot/efi")
               (keyboard-layout keyboard-layout)))

  (kernel-arguments '("zswap.enabled=1"
                      "zswap.compressor=lz4"
                      "zswap.zpool=z3fold"))

  (file-systems (cons* (file-system
                         (device (file-system-label "ROOT"))
                         (mount-point "/")
                         (type "btrfs")
                         (options "autodefrag,compress-force=zstd,discard,ssd_spread"))
                       (file-system
                         (device (file-system-label "DATA")
                                 #;(uuid "a043cca4-fb76-43a0-b12b-008bf126dfaa"))
                         (mount-point "/home")
                         (type "btrfs")
                         (options "autodefrag,compress-force=zstd,discard")
                         (flags '(no-exec no-suid))
                         (create-mount-point? #t))
                       (file-system
                         (device (file-system-label "EFI"))
                         (mount-point "/boot/efi")
                         (type "vfat"))
                       %base-file-systems))

  (users (cons (user-account
                (name "lafrenierejm")
                (comment "Joseph LaFreniere")
                (group "users")
                (supplementary-groups
                 '("adbusers"
                   "audio"
                   "kvm"
                   "libvirt"
                   "lp"
                   "netdev"
                   "video"
                   "wheel")))
               %base-user-accounts))

  ;; Specify system-wide packages.
  (packages (cons*
             btrfs-progs
             ccache
             compsize
             cups
             emacs-exwm
             ghostscript
             hplip
             i3-wm
             nss-certs           ; HTTPS
             ntp
             openntpd
             xinit
             xf86-input-libinput
             xf86-video-amdgpu   ; nonfree
             xorg-server
             virt-manager
             %base-packages))

  (services (cons*
             (bluetooth-service #:auto-enable? #t)
             (service cups-service-type
                      (cups-configuration
                       (default-paper-size "letterpaper")
                       (extensions (list cups-filters hplip-minimal))))
             (service libvirt-service-type
                      (libvirt-configuration
                       (unix-sock-group "libvirt")))
             (udev-rules-service
              'android android-udev-rules
              #:groups '("adbusers"))
             (service qemu-binfmt-service-type
                      (qemu-binfmt-configuration
                       (platforms (lookup-qemu-platforms "arm" "aarch64"))
                       (guix-support? #t)))
             (simple-service 'wireguard-module
                             kernel-module-loader-service-type
                             '("wireguard"))
             (service slim-service-type)
             (remove
              (lambda (service)
                (let* ((type (service-kind service))
                       (name (service-type-name type)))
                  (or (memq type
                            (list gdm-service-type
                                  modem-manager-service-type
                                        ; network-manager-service-type
                                  screen-locker-service-type))
                      (memq name '(network-manager-applet)))))
              %desktop-services)))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
