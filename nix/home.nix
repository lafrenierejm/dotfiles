{
  inputs,
  pkgs,
  pkgsTrunk,
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
  vcIgnores = [
    "*#" # emacs
    "*.elc" # emacs
    "*~" # emacs
    ".DS_Store" # darwin
    ".dir-locals.el" # emacs
    ".direnv/" # direnv
    ".mono/" # mono
    ".venv/" # python
  ];
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
          inputs.ghq.packages."${system}".ghq
        ]
        (with pkgs; [
          (aspellWithDicts (aspellDicts: (with aspellDicts; [en en-computers])))
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
          efm-langserver
          exiftool
          eza
          fd
          findutils # GNU `find` and `xargs`
          gh
          git-crypt
          git-filter-repo
          gitAndTools.gitFull
          gitstatus
          gnupg
          hyperfine
          id3v2
          ispell
          isync
          jaq
          jotdown
          mosh
          mpv
          mu
          nixd
          nodePackages.bash-language-server
          nodePackages.graphql-language-service-cli
          nodePackages.pnpm
          nodePackages.prettier
          nodePackages.typescript-language-server
          nodePackages.vscode-json-languageserver
          nodePackages.yaml-language-server
          opentofu
          pre-commit
          pyright
          python3
          ripgrep
          rsync
          rust-analyzer
          shfmt
          subversion
          terraform-docs
          terraform-ls
          tflint
          typos
          unzip
          uv
          vale
          vale-ls
          yt-dlp
        ])
        (with pkgsTrunk; [
          dockerfmt
        ])
        (lib.lists.optionals personal [
          pkgs.imagemagick
          (lib.lists.optionals pkgs.stdenv.isLinux (with pkgs; [
            beets
            ffmpeg-headless
            inkscape
            nicotine-plus
            picard
          ]))
        ])
        (lib.lists.optionals pkgs.stdenv.isDarwin (with pkgs; [
          skhd
        ]))
        (lib.lists.optionals pkgs.stdenv.isLinux (with pkgs; [
          bitwarden
          dconf2nix
          pavucontrol
          signal-desktop-source
          transmission_4-gtk
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
          jq = "jaq";
          la = "eza --long --git --time-style=long-iso --all";
          ll = "eza --long --git --time-style=long-iso";
        }
        (lib.attrsets.optionalAttrs pkgs.stdenv.isDarwin {
          emacs = "${programs.emacs.package}/Applications/Emacs.app/Contents/MacOS/Emacs";
        })
      ];
      username = userName;
    }
    (lib.attrsets.optionalAttrs pkgs.stdenv.isDarwin {
      file."Library/Application Support/vale/.vale.ini".source = ../vale/.vale.ini;
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
      ];
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };

    chromium = {
      enable = pkgs.stdenv.isLinux;
      package = pkgs.ungoogled-chromium;
      dictionaries = [pkgs.hunspellDictsChromium.en_US];
      extensions = [
        {id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";} # ublock origin
        {id = "nngceckbapebfimnlniiiahkandclblb";} # Bitwarden
      ];
    };

    emacs = {
      enable = true;
      package =
        (
          pkgs.emacs-git-pgtk.overrideAttrs (old: {
            withTreeSitter = true;
            withNativeCompilation = !pkgs.stdenv.isDarwin;
            passthru = old.passthru // {treeSitter = true;};
            patches =
              (old.patches or [])
              ++ (lib.lists.optionals pkgs.stdenv.isDarwin (
                map (patchFilename: inputs.emacs-plus + "/patches/${patchFilename}") [
                  "emacs-31/system-appearance.patch"
                ]
              ));
          })
        ).override {
          # https://github.com/NixOS/nixpkgs/issues/395169
          withNativeCompilation = !pkgs.stdenv.isDarwin;
        };
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
          difftastic
          dired-collapse
          dired-narrow
          dired-subtree
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
          gptel
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
          ivy-hydra
          ivy-pass
          ivy-rich
          ivy-yasnippet
          jinja2-mode
          journalctl-mode
          json-mode
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
          origami
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
          # ws-butler
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
          default = "ddg";
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

    ghostty = {
      enable = pkgs.stdenv.isLinux;
      enableBashIntegration = true;
      enableFishIntegration = true;
      enableZshIntegration = true;
      settings = {
        window-decoration = false;
        gtk-titlebar = false;
      };
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
          "git@codeberg.org:" = {
            insteadOf = "https://codeberg.org/";
          };
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
      ignores = vcIgnores;
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

    mercurial = {
      enable = true;
      ignores = vcIgnores;
      userEmail = "hg@lafreniere.xyz";
      userName = realName;
    };

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
      package = pkgsTrunk.rbw;
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

    ripgrep = {
      enable = true;
      arguments = [
        "--hidden"
        "--glob=!.git/*" # exclude .git directories
      ];
    };

    ripgrep-all = {
      enable = true;
      package = inputs.ripgrep-all.packages."${system}".rga;
      custom_adapters = [
        {
          name = "gron";
          version = 1;
          description = "Transform JSON into discrete JS assignments";
          extensions = ["json"];
          mimetypes = ["application/json"];
          binary = "${
            inputs.gron.packages."${system}".gronWithFallback
          }/bin/gron-with-fallback";
          disabled_by_default = false;
          match_only_by_mime = false;
        }
      ];
    };

    ssh = {
      enable = true;
      compression = true;
      matchBlocks = {
        "*" = {
          host = "*";
          identitiesOnly = true;
          identityFile = "~/.ssh/id_ed25519";
        };
        "github.com" = {
          hostname = "github.com";
          user = "lafrenierejm";
        };
        "gitlab.com" = {
          hostname = "gitlab.com";
          user = "lafrenierejm";
        };
        "git.sr.ht" = {
          hostname = "git.sr.ht";
          user = "lafrenierejm";
        };
        "earthbound" = {
          hostname = "earthbound.fin-alioth.ts.net";
          user = "lafrenierejm";
        };
      };
    };

    zsh = {
      enable = true;
      autocd = true;
      autosuggestion.enable = true;
      defaultKeymap = "emacs";
      initContent = lib.concatStringsSep "\n" (lib.lists.flatten [
        ''. "$HOME/.config/zsh/prompt.zsh"''
        (lib.lists.optionals (!personal) [
          pyenvEnable
          voltaEnable
        ])
        ''
          # Load NVM.
          export NVM_DIR="$HOME/.nvm"
          [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && . "/opt/homebrew/opt/nvm/nvm.sh"
          [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && . "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"''
      ]);
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

  xdg = {
    enable = pkgs.stdenv.isLinux;
    userDirs = {
      enable = pkgs.stdenv.isLinux;
      createDirectories = true;
    };
  };
}
