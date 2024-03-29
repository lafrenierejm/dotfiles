;; This "manifest" file can be passed to 'guix package -m' to reproduce
;; the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to
;; capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (guix transformations))

(define emacs-next-pgtk
  (options->transformation
   '((with-input . "emacs=emacs-next-pgtk"))))

(packages->manifest
 (map emacs-next-pgtk
      (list
       (specification->package "abcde")
       (specification->package "adb")
       (specification->package "autorandr")
       (specification->package "clojure")
       (specification->package "clojure-tools-cli")
       (specification->package "coreutils")
       (specification->package "curl")
       (specification->package "dmenu")
       (specification->package "dosfstools")
       (specification->package "emacs-ace-window")
       (specification->package "emacs-adoc-mode")
       (specification->package "emacs-aggressive-indent")
       (specification->package "emacs-auth-source-pass")
       (specification->package "emacs-browse-at-remote")
       (specification->package "emacs-caps-lock")
       (specification->package "emacs-cascading-dir-locals")
       (specification->package "emacs-cider")
       (specification->package "emacs-clojure-mode")
       (specification->package "emacs-company")
       (specification->package "emacs-company-posframe")
       (specification->package "emacs-company-restclient")
       (specification->package "emacs-counsel")
       (specification->package "emacs-counsel-projectile")
       (specification->package "emacs-counsel-tramp")
       (specification->package "emacs-desktop-environment")
       (specification->package "emacs-diff-hl")
       (specification->package "emacs-dired-hacks")
       (specification->package "emacs-docker")
       (specification->package "emacs-docker-tramp")
       (specification->package "emacs-dockerfile-mode")
       (specification->package "emacs-dtrt-indent")
       (specification->package "emacs-editorconfig")
       (specification->package "emacs-elfeed")
       (specification->package "emacs-elfeed-org")
       (specification->package "emacs-emms")
       (specification->package "emacs-elpy")
       (specification->package "emacs-evil")
       (specification->package "emacs-evil-cleverparens")
       (specification->package "emacs-evil-collection")
       (specification->package "emacs-evil-indent-plus")
       (specification->package "emacs-evil-matchit")
       (specification->package "emacs-evil-org")
       (specification->package "emacs-evil-surround")
       (specification->package "emacs-exec-path-from-shell")
       (specification->package "emacs-feature-mode")
       (specification->package "emacs-flx")
       (specification->package "emacs-flycheck")
       (specification->package "emacs-forge")
       (specification->package "emacs-form-feed")
       (specification->package "emacs-frames-only-mode")
       (specification->package "emacs-geiser")
       (specification->package "emacs-geiser-guile")
       (specification->package "emacs-general")
       (specification->package "emacs-ghq")
       (specification->package "emacs-go-mode")
       (specification->package "emacs-guix")
       (specification->package "emacs-haskell-mode")
       (specification->package "emacs-hcl-mode")
       (specification->package "emacs-helpful")
       (specification->package "emacs-highlight-indent-guides")
       (specification->package "emacs-hy-mode")
       (specification->package "emacs-ivy")
       (specification->package "emacs-ivy-pass")
       (specification->package "emacs-ivy-rich")
       (specification->package "emacs-ivy-yasnippet")
       (specification->package "emacs-jinja2-mode")
       (specification->package "emacs-keystore-mode")
       (specification->package "emacs-lsp-mode")
       (specification->package "emacs-magit")
       (specification->package "emacs-minibuffer-line")
       (specification->package "emacs-minions")
       (specification->package "emacs-mixed-pitch")
       (specification->package "emacs-modus-themes")
       (specification->package "emacs-monky")
       (specification->package "emacs-next-pgtk")
       (specification->package "emacs-nix-mode")
       (specification->package "emacs-no-littering")
       (specification->package "emacs-org")
       (specification->package "emacs-pdf-tools")
       (specification->package "emacs-perspective")
       (specification->package "emacs-pinentry")
       (specification->package "emacs-polymode")
       (specification->package "emacs-polymode-ansible")
       (specification->package "emacs-polymode-markdown")
       (specification->package "emacs-polymode-org")
       (specification->package "emacs-powershell")
       (specification->package "emacs-projectile")
       (specification->package "emacs-python-black")
       (specification->package "emacs-pyvenv")
       (specification->package "emacs-racket-mode")
       (specification->package "emacs-rainbow-delimiters")
       (specification->package "emacs-rainbow-identifiers")
       (specification->package "emacs-reformatter")
       (specification->package "emacs-relative-buffers")
       (specification->package "emacs-restclient")
       (specification->package "emacs-rust-mode")
       (specification->package "emacs-semantic-refactor")
       (specification->package "emacs-slime")
       (specification->package "emacs-smartparens")
       (specification->package "emacs-ssh-agency")
       (specification->package "emacs-standard-dirs")
       (specification->package "emacs-terraform-mode")
       (specification->package "emacs-toml-mode")
       (specification->package "emacs-tramp")
       (specification->package "emacs-transmission")
       (specification->package "emacs-use-package")
       (specification->package "emacs-vterm")
       (specification->package "emacs-webpaste")
       (specification->package "emacs-which-key")
       (specification->package "emacs-ws-butler")
       (specification->package "emacs-wucuo")
       (specification->package "emacs-yaml-mode")
       (specification->package "emacs-yasnippet")
       (specification->package "emacs-youtube-dl")
       (specification->package "exa")
       (specification->package "exfatprogs")
       (specification->package "fd")
       (specification->package "feh")
       (specification->package "ffmpeg")
       (specification->package "file")
       (specification->package "firefox")
       (specification->package "font-adobe-source-code-pro")
       (specification->package "font-google-noto")
       (specification->package "ghq")
       (specification->package "git")
       (specification->package "git-crypt")
       (specification->package "gnupg")
       (specification->package "guile")
       (specification->package "guix")
       (specification->package "i3status")
       (specification->package "isync")
       (specification->package "kitty")
       (specification->package "leiningen")
       (specification->package "libfdk")
       (specification->package "mcron")
       (specification->package "msmtp")
       (specification->package "mu")
       (specification->package "neovim")
       (specification->package "ntfs-3g")
       (specification->package "openjdk")
       (specification->package "openssh")
       (specification->package "password-store")
       (specification->package "pinentry")
       (specification->package "python")
       (specification->package "rclone")
       (specification->package "redshift")
       (specification->package "ripgrep")
       (specification->package "syncthing")
       (specification->package "transmission")
       (specification->package "ublock-origin-chromium")
       (specification->package "udiskie")
       (specification->package "ungoogled-chromium")
       (specification->package "unzip")
       (specification->package "vorbis-tools")
       (specification->package "weechat")
       (specification->package "wireguard-tools")
       (specification->package "xcape")
       (specification->package "xmodmap")
       (specification->package "xprop"))))
