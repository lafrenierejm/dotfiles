{
  inputs,
  pkgs,
  pkgsUnstable,
  lib,
  system,
  userName,
  gitEmail,
  gitUseGpg,
  personal,
  realName,
  ...
}: let
  homeDirectory =
    if pkgs.stdenv.isDarwin
    then "/Users/${userName}"
    else "/home/${userName}";
  pinentryPkg =
    if pkgs.stdenv.isDarwin
    then pkgs.pinentry_mac
    else pkgs.pinentry-gnome3;
  pinentryBin =
    if pkgs.stdenv.isDarwin
    then "${pinentryPkg}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac"
    else "${pinentryPkg}/bin/pinentry";
  pyenvEnable = pkgs.lib.readFile ../sh/pyenv.sh;
  voltaEnable = pkgs.lib.readFile ../sh/volta.sh;
in rec {
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home = lib.attrsets.mergeAttrsList [
    {
      stateVersion = "23.05";
      file.".gnupg/gpg-agent.conf".text =
        lib.concatStringsSep "\n"
        (lib.attrsets.mapAttrsToList (name: value: name + " " + toString value) {
          pinentry-program = pinentryBin;
          default-cache-ttl = 7200;
          default-cache-ttl-ssh = 7200;
        });
      packages = lib.lists.flatten [
        pinentryPkg
        [
          inputs.gron.packages."${system}".gron
          inputs.ripgrep-all.packages."${system}".rga
        ]
        (with pkgs; [
          (aspellWithDicts (aspellDicts: (with aspellDicts; [en en-computers en-science])))
          atool
          aws-sso-creds
          awscli2
          babashka
          bfg-repo-cleaner
          cachix
          clang-tools
          clojure
          coreutils
          curl
          dos2unix
          eza
          fd
          gh
          ghq
          git-crypt
          git-filter-repo
          gitAndTools.gitFull
          gnupg
          gojq
          id3v2
          ispell
          isync
          isync
          mosh
          mpv
          mu
          nixd
          nodePackages.bash-language-server
          nodePackages.graphql-language-service-cli
          nodePackages.prettier
          nodePackages.typescript-language-server
          nodePackages.vscode-json-languageserver
          nodePackages.yaml-language-server
          opentofu
          pre-commit
          pyright
          ripgrep
          rsync
          rust-analyzer
          subversion
          terraform-docs
          terraform-ls
          typos
          unzip
          uv
          yt-dlp
        ])
        (lib.lists.optionals pkgs.stdenv.isDarwin (with pkgs; [
          skhd
        ]))
        (lib.lists.optionals pkgs.stdenv.isLinux (with pkgs; [
          bitwarden
          dconf2nix
          signal-desktop
          transmission-gtk
          ungoogled-chromium
          zoom
        ]))
        (pkgs.lib.lists.optionals (!personal) (with pkgs; [
          groovy
          nodejs
          # vault
        ]))
      ];
      sessionPath = [
        "$HOME/.local/bin"
      ];
      sessionVariables = {
        DIRENV_LOG_FORMAT = "";
        NIX_PATH = "nixpkgs=${inputs.nixpkgs}";
      };
      shellAliases = lib.attrsets.mergeAttrsList [
        {
          aws-ecr-login = ''
            aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin "$(aws sts get-caller-identity | jq -r '.Account').dkr.ecr.us-east-1.amazonaws.com"'';
          extract = "atool --extract --explain --subdir";
          jq = "gojq";
          la = "eza --long --git --time-style=long-iso --all";
          ll = "eza --long --git --time-style=long-iso";
        }
        (lib.attrsets.optionalAttrs pkgs.stdenv.isDarwin {
          emacs = "${programs.emacs.package}/Applications/Emacs.app/Contents/MacOS/Emacs";
        })
      ];
      username = userName;
    }
    (lib.attrsets.optionalAttrs pkgs.stdenv.isLinux {
      pointerCursor = {
        name = "Adwaita";
        package = pkgs.gnome.adwaita-icon-theme;
        size = 24;
        x11 = {
          enable = true;
          defaultCursor = "Adwaita";
        };
      };
    })
  ];

  accounts.email = {
    maildirBasePath = "Mail";
    accounts = let
      signature = {
        showSignature = true;
        text = pkgs.lib.concatStringSep [
          "-- "
          realName
        ];
      };
    in {
      "xyz.lafreniere" = let
        domain = "fastmail.com";
        userName = "lafrenierejm@${domain}";
      in {
        inherit signature realName userName;
        address = "git@lafreniere.xyz";
        flavor = "fastmail.com";
        mujmap = {
          enable = true;
          notmuchSetupWarning = true;
          settings = {
            auto_create_new_mailboxes = true;
            session_url = "https://api.${domain}/jmap/session";
          };
        };
        passwordCommand = [
          "rbw"
          "get"
          "--field"
          "api_mujmap"
          "${domain}/${userName}"
        ];
        notmuch.enable = true;
        primary = true;
      };
      "com.gmail" = let
        address = "lafrenierejm@gmail.com";
      in {
        inherit address signature realName;
        userName = address;
        flavor = "gmail.com";
        lieer = {
          enable = true;
          notmuchSetupWarning = true;
          sync.enable = true;
        };
        passwordCommand = [
          "rbw"
          "get"
          "--field"
          "api_gmail"
          "cloud.google.com/${address}/lieer"
        ];
        notmuch.enable = true;
      };
    };
  };

  programs = {
    bash = {
      enable = true;
      enableCompletion = true;
      enableVteIntegration = true;
      historyControl = [
        # From https://github.com/CeleritasCelery/emacs-native-shell-complete#bash:
        # > If the HISTCONTROL environment variable is not set to ignorespace or
        # > ignoreboth you will get a lot of garbage in your shell history.
        "erasedups"
        "ignorespace"
      ];
      initExtra = lib.concatStringsSep "\n" [
        # From https://github.com/CeleritasCelery/emacs-native-shell-complete#bash:
        # > We [...] need to disable bracketed-paste.
        "set enable-bracketed-paste off"
        # Enable extended globbing patterns.
        "shopt -s extglob"
        # Enable non-Nix programs.
        pyenvEnable
        voltaEnable
      ];
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };

    emacs = {
      enable = true;
      package =
        inputs
        .emacs-overlay
        .packages
        ."${system}"
        .emacs-pgtk
        .overrideAttrs (old: {
          withTreeSitter = true;
          withNativeCompilation = true;
          passthru =
            old.passthru
            // {
              treeSitter = true;
            };
          # Use `nix-prefetch-url` to get the below shasums.
          patches =
            (old.patches or [])
            ++ (lib.lists.optionals pkgs.stdenv.isDarwin (
              let
                host = "raw.githubusercontent.com";
                owner = "d12frosted";
                repo = "homebrew-emacs-plus";
                commit = "ff462afe5e971411e9738305ac6afd67a0e041bb";
                patchUrl = path: "https://${host}/${owner}/${repo}/${commit}/${path}";
              in [
                # Fix window role for compatibility with yabai.
                (pkgs.fetchpatch {
                  url = patchUrl "patches/emacs-28/fix-window-role.patch";
                  sha256 = "0c41rgpi19vr9ai740g09lka3nkjk48ppqyqdnncjrkfgvm2710z";
                })
                # Make Emacs aware of OS-level light/dark mode.
                (pkgs.fetchpatch {
                  url = patchUrl "patches/emacs-30/system-appearance.patch";
                  sha256 = "1dkx8xc3v2zgnh6fpx29cf6kc5h18f9misxsdvwvy980cj0cxcwy";
                })
                # Use poll instead of select to get file descriptors.
                (pkgs.fetchpatch {
                  url = patchUrl "patches/emacs-30/poll.patch";
                  sha256 = "0988ysajww160wms05hydg48kyd4xlwn6if27dcf9n3g5vw7ds2r";
                })
              ]
            ));
        });
      extraPackages = epkgs:
        with epkgs; [
          adoc-mode
          aggressive-indent
          ahk-mode
          ansible
          apples-mode
          auctex
          browse-at-remote
          buffer-name-relative
          caps-lock
          cascading-dir-locals
          chatgpt-shell
          cider
          cider-hydra
          clj-refactor
          clojure-mode
          company
          company-posframe
          company-restclient
          compdef
          counsel
          counsel-notmuch
          counsel-projectile
          counsel-tramp
          counsel-web
          deadgrep
          desktop-environment
          diff-hl
          dired-collapse
          dired-narrow
          dired-subtree
          docker
          dockerfile-mode
          dtrt-indent
          dwim-shell-command
          editorconfig
          elfeed
          elfeed-org
          envrc
          evil
          evil-cleverparens
          evil-collection
          evil-indent-plus
          evil-matchit
          evil-org
          evil-surround
          evil-tex
          evil-textobj-tree-sitter
          exec-path-from-shell
          f
          feature-mode
          fennel-mode
          file-info
          flx
          forge
          form-feed
          frames-only-mode
          geiser-guile
          general
          ghq
          go-eldoc
          go-mode
          gorepl-mode
          graphql-ts-mode
          groovy-mode
          guix
          haskell-mode
          hcl-mode
          helpful
          hindent
          hy-mode
          indent-bars
          inf-ruby
          ivy
          ivy-pass
          ivy-rich
          ivy-yasnippet
          jinja2-mode
          journalctl-mode
          json-mode
          keystore-mode
          lua-mode
          magit
          markdown-mode
          minions
          modus-themes
          monky
          nix-mode
          no-littering
          nodejs-repl
          notmuch
          nov
          package-lint
          password-store
          pdf-tools
          poly-ansible
          poly-markdown
          poly-org
          polymode
          powershell
          projectile
          pytest
          python
          racket-mode
          rainbow-delimiters
          rainbow-identifiers
          rbenv
          reformatter
          restclient
          rust-mode
          seeing-is-believing
          shadowenv
          shell-maker
          shfmt
          slime
          smart-dash
          smartparens
          sqlite3
          sqlup-mode
          standard-dirs
          terraform-mode
          toml-mode
          tree-sitter
          treesit-grammars.with-all-grammars
          vterm
          webpaste
          which-key
          ws-butler
          wucuo
          x509-mode
          xonsh-mode
          yaml-mode
          yasnippet
          youtube-dl
        ];
    };

    firefox = {
      enable = personal && pkgs.stdenv.isLinux;
      package = pkgs.firefox-bin;
      profiles."personal.default" = {
        id = 0;
        name = "personal";
        isDefault = true;
        userChrome = pkgs.lib.readFile ../firefox/chrome/userChrome.css;
        search = {
          default = "DuckDuckGo";
          force = true;
        };
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        };
      };
    };

    fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    git = {
      enable = true;
      package = pkgs.gitAndTools.gitFull;
      aliases = {
        aban = "checkout --";
        abanp = "checkout -p --";
        "abort" = "rebase --abort";
        alias = "! git config --get-regexp '^alias.' | sed -e 's/^alias.//' -e 's/ / = /'";
        amend = "commit --amend";
        cloner = "clone --recursive";
        cont = "rebase --continue";
        delete = ''!git branch -d "$1" ; git push "$2" --delete "$1" #'';
        dif = "diff --word-diff=color";
        fixup = "commit --amend -C HEAD";
        head = "log -1 HEAD";
        pop = "stash pop";
        pure = "pull --rebase";
        review = "diff --word-diff=color --staged";
        st = "status -sb";
        unstage = "reset HEAD --";
      };
      extraConfig = {
        init.defaultBranch = "main";
        fetch = {
          prune = true;
          pruneTags = true;
        };
        ghq.root = "${homeDirectory}/Documents";
        github.user = "lafrenierejm";
        gitlab.user = "lafrenierejm";
        pull = {
          autostash = true;
          ff = "only";
          prune = true;
          pruneTags = true;
          tags = true;
        };
        push = {
          gpgSign =
            if gitUseGpg
            then "if-asked"
            else "false";
        };
        rebase.autoStash = true;
        sendemail = {
          from = "Joseph LaFreniere <${gitEmail}>";
          smtpuser = "";
          smtpserver = "${pkgs.msmtp}/bin/msmtp";
          smtpencryption = "tls";
          chainreplyto = false;
          confirm = "auto";
        };
        url = {
          "git@github.com:" = {
            insteadOf = "https://github.com/";
          };
          "git@gitlab.com:" = {
            insteadOf = "https://gitlab.com/";
          };
          "git@git.sr.ht:" = {
            insteadOf = "https://git.sr.ht/";
          };
        };
      };
      difftastic.enable = true;
      ignores = [
        # emacs
        "*#"
        "*.elc"
        "*~"
        ".dir-locals.el"
        # direnv
        ".direnv/"
        # mono
        ".mono/"
        # python
        ".venv/"
      ];
      lfs.enable = true;
      signing =
        if gitUseGpg
        then {
          key = "0375DD9AEDD168A3ADA39EBAEE236AA0141EFCA3"; # pragma: allowlist secret
          signByDefault = true;
        }
        else null;
      userEmail = gitEmail;
      userName = realName;
    };

    home-manager.enable = true;

    htop = {
      enable = true;
      settings.show_program_path = true;
    };

    lieer.enable = personal;

    mujmap = {
      enable = personal;
      package = inputs.mujmap.packages."${system}".mujmap;
    };

    notmuch.enable = personal;

    pandoc.enable = true;

    rbenv = {
      enable = true;
      enableZshIntegration = true;
    };

    rbw = {
      enable = true;
      package = pkgsUnstable.rbw;
      settings = {
        base_url = "https://vault.bitwarden.com";
        identity_url = "https://identity.bitwarden.com";
        notifications_url = "https://notifications.bitwarden.com";
        lock_timeout = 7200;
        sync_interval = 3600;
        email = "bitwarden.com@lafreniere.xyz";
        pinentry = pinentryPkg;
      };
    };

    ripgrep-all = {
      enable = true;
      package = inputs.ripgrep-all.packages."${system}".rga;
      customAdapters = [
        {
          name = "gron";
          version = 1;
          description = "Transform JSON into discrete JS assignments";
          extensions = ["json"];
          mimetypes = ["application/json"];
          binary = "${
            inputs.gron.packages."${system}".gronWithFallback
          }/bin/gron-with-fallback";
          disabledByDefault = false;
          matchOnlyByMime = false;
        }
      ];
    };

    ssh = {
      enable = true;
      compression = true;
      matchBlocks = {
        "github.com" = {
          hostname = "github.com";
          user = "lafrenierejm";
          identityFile = "~/.ssh/id_ed25519";
          identitiesOnly = true;
        };
        "gitlab.com" = {
          hostname = "gitlab.com";
          user = "lafrenierejm";
          identityFile = "~/.ssh/id_ed25519";
          identitiesOnly = true;
        };
        "git.sr.ht" = {
          hostname = "git.sr.ht";
          user = "lafrenierejm";
          identityFile = "~/.ssh/id_ed25519";
          identitiesOnly = true;
        };
        "earthbound" = {
          hostname = "10.0.0.53";
          user = "lafrenierejm";
          identityFile = "~/.ssh/id_ed25519";
          identitiesOnly = true;
        };
      };
    };

    zsh = {
      enable = true;
      autocd = true;
      autosuggestion.enable = true;
      defaultKeymap = "emacs";
      initExtra = lib.concatStringsSep "\n" [
        ''. "$HOME/.config/zsh/vterm.zsh"''
        pyenvEnable
        voltaEnable
        # nvm
        ''
          # Load NVM.
          export NVM_DIR="$HOME/.nvm"
          [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && . "/opt/homebrew/opt/nvm/nvm.sh"
          [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && . "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"''
      ];
      profileExtra = lib.concatStringsSep "\n" [
        ''export PATH="$PATH:$HOME/.dotnet/tools"''
      ];
    };
  };

  services = {
    emacs = {
      enable = pkgs.stdenv.isLinux;
      client.enable = true;
      client.arguments = ["--create-frame"];
      defaultEditor = true;
    };
  };
}
