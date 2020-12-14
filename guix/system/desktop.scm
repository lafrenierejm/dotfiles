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
 ghostscript
 gnupg
 linux
 ntp
 scanner
 xorg
 virtualization)

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

  (file-systems (append
                 (list (file-system
                         (device (file-system-label "ROOT"))
                         (mount-point "/")
                         (type "btrfs")
                         (options "autodefrag,compress-force=zstd,discard,ssd_spread"))
                       (file-system
                         (device (file-system-label "EFI"))
                         (mount-point "/boot/efi")
                         (type "vfat")))
                 %base-file-systems))

  (users (cons (user-account
                (name "lafrenierejm")
                (comment "Joseph LaFreniere")
                (group "users")
                (supplementary-groups '("wheel" "netdev" "kvm" "lp" "libvirt" "audio" "video")))
               %base-user-accounts))

  ;; This is where we specify system-wide packages.
  (packages (append (list
             btrfs-progs
             ccache
             compsize
             cups
             ghostscript
             hplip
             nss-certs ; HTTPS
             ntp
             openntpd
             xinit
             xf86-input-libinput
             xf86-video-amdgpu
             xorg-server
             virt-manager)
             %base-packages))

  ;; Add GNOME and Xfce---we can choose at the log-in screen
  ;; by clicking the gear.  Use the "desktop" services, which
  ;; include the X11 log-in service, networking with
  ;; NetworkManager, and more.
  (services (cons*
             (service cups-service-type
                      (cups-configuration
                       (default-paper-size "letterpaper")
                       (extensions (list cups-filters hplip-minimal))))
             (service libvirt-service-type
                      (libvirt-configuration
                       (unix-sock-group "libvirt")))
             (simple-service 'custom-udev-rules udev-service-type
                             (list sane-backends android-udev-rules))
             (service qemu-binfmt-service-type
                      (qemu-binfmt-configuration
                       (platforms (lookup-qemu-platforms "arm" "aarch64"))
                       (guix-support? #t)))
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
                      (memq name
                            '(network-manager-applet)))))
              %desktop-services)))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
