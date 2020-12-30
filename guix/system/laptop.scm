;; To apply:
;; sudo -E guix system -L ~/.config/guix/system reconfigure ~/.config/guix/system/odyssey.scm

(define-module (odyssey.lafreniere.xyz)
  #:use-module (gnu)
  #:use-module (gnu packages certs)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu system nss)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu services kernel-modules))

(define %odyssey/xorg-touchpad
  "\
Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ClickMethod\" \"clickfinger\"
  Option \"ScrollMethod\" \"twofinger\"
  Option \"NaturalScrolling\" \"true\"
EndSection")

(define %odyssey/udev-backlight-rule
  ;; Allow members of the "video" group to change the screen brightness.
  (udev-rule
   "90-backlight.rules"
   (string-append
    "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
    "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
    "\n"
    "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
    "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define %odyssey/services
  (cons* (load-broadcom-sta-service)
         (modify-services %desktop-services
           (udev-service-type
            config =>
            (udev-configuration
             (inherit config)
             (rules (append (udev-configuration-rules config)
                            (list %odyssey/udev-backlight-rule)))))
           (gdm-service-type
            config =>
            (gdm-configuration
             (inherit config)
             (default-user "lafrenierejm")
             (xorg-configuration
              (xorg-configuration
               (modules %default-xorg-modules)
               (extra-config (list %odyssey/xorg-touchpad)))))))))

(operating-system
  (kernel linux)
  ;; Blacklist conflicting kernel modules.
  (kernel-arguments
   '("modprobe.blacklist=b43,b43legacy,ssb,bcm43xx,brcm80211,brcmfmac,brcmsmac,bcma"))
  (firmware (cons* iwlwifi-firmware %base-firmware))
  (host-name "odyssey.lafreniere.xyz")
  (timezone "America/Chicago")
  (locale "en_US.utf8")

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (target "/boot/efi")))

  ;; Specify a mapped device for the encrypted root partition.
  ;; The UUID is that returned by 'cryptsetup luksUUID'.
  (mapped-devices
   (list (mapped-device
          (source "/dev/sda2")
          (target "root")
          (type luks-device-mapping))))

  (file-systems
   (cons* (file-system
            (device (file-system-label "root"))
            (mount-point "/")
            (type "btrfs")
            (dependencies mapped-devices))
          (file-system
            (device "/dev/sda1")
            (mount-point "/boot/efi")
            (type "vfat")
            (needed-for-boot? #t)
            (create-mount-point? #t))
          %base-file-systems))

  (users
   (cons (user-account
          (name "lafrenierejm")
          (comment "Joseph LaFreniere")
          (group "users")
          (supplementary-groups
           '("wheel" "netdev" "audio" "video"))
          (home-directory "/home/lafrenierejm"))
         %base-user-accounts))

  ;; This is where we specify system-wide packages.
  (packages
   (cons* nss-certs ; for HTTPS access
          %base-packages))

  ;; Add GNOME and/or Xfce---we can choose at the log-in
  ;; screen with F1.  Use the "desktop" services, which
  ;; include the X11 log-in service, networking with
  ;; NetworkManager, and more.
  (services %odyssey/services)

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
