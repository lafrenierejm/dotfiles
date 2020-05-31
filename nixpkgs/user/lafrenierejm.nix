{ config, lib, pkgs, ... }:

with import <nixpkgs> {
  # use the latest Emacs overlay
  overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];
};

{
  imports = [
    ../service/lorri/default.nix
    ../service/mpd/default.nix
  ];

  home.file = {
    ".emacs.d" = {
      source = ../../emacs;
      recursive = true;
    };
    ".hgrc" = {
      source = ../../hg/hgrc;
    };
    ".xinitrc" = {
      source = ../../X11/xinitrc;
    };
    ".xprofile" = {
      source = ../../X11/xinitrc;
    };
  };

  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    direnv
    dnsutils
    file
    firefox
    git
    gnupg
    id3v2
    ispell
    isync
    lorri
    mosh
    mpv
    mu
    nixpkgs-fmt
    pass
    pinentry
    redshift
    ripgrep
    udisks
    xcape
    youtube-dl
  ];

  home.sessionVariables = {
    EDITOR = "emacsclient --alternate-editor='' --no-wait";
    XDG_CONFIG_HOME = "${config.home.homeDirectory}/etc";
  };

  programs.emacs = {
    enable = true;
    package =
      (pkgs.emacs.override {
        # Use GTK3 instead of the default GTK2
        withGTK2 = false;
        withGTK3 = true;
      }).overrideAttrs (attrs: {
        # The desktop file is useless to me.
        postInstall =
          (attrs.postInstall or "") + "rm $out/share/applications/emacs.desktop";
      });
    extraPackages = epkgs: (with epkgs.melpaPackages; [
      ace-window
      adoc-mode
      aggressive-indent
      ansible-vault
      apples-mode
      auth-source-pass
      browse-at-remote
      cider
      cider-hydra
      clj-refactor
      clojure-mode
      company
      company-posframe
      company-restclient
      counsel
      counsel-projectile
      counsel-tramp
      desktop-environment
      diff-hl
      dired-collapse
      dired-narrow
      dired-subtree
      docker
      docker-tramp
      editorconfig
      elfeed
      elfeed-org
      emms
      evil
      evil-cleverparens
      evil-collection
      evil-indent-plus
      evil-magit
      evil-matchit
      evil-org
      evil-surround
      exwm-firefox-evil
      f
      flx
      flycheck
      forge
      form-feed
      geiser
      general
      gitattributes-mode
      groovy-mode
      hackernews
      helpful
      highlight-indent-guides
      hy-mode
      ivy
      ivy-pass
      ivy-rich
      ivy-yasnippet
      json-mode
      magit
      markdown-mode
      md4rd
      minions
      mixed-pitch
      modus-operandi-theme
      monky
      nix-mode
      no-littering
      password-store
      pdf-tools
      poly-org
      polymode
      projectile
      perspective
      racket-mode
      rainbow-delimiters
      rainbow-identifiers
      reformatter
      restclient
      rust-mode
      slime
      smartparens
      sqlup-mode
      ssh-agency
      swiper
      sx
      toml-mode
      use-package
      vterm
      ws-butler
      wucuo
      yaml-mode
      yasnippet
    ]) ++ (with epkgs.elpaPackages; [
      auctex
      caps-lock
      exwm
      minibuffer-line
    ]);
  };

  services.emacs = {
    enable = true;
  };

  services.lorri = {
    enable = true;
  };
}
