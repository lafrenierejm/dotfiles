{
  inputs,
  lib,
  config,
  pkgs,
  system,
  username,
  gitEmail,
  gitUseGpg,
  personal,
  ...
}: let
  homeDirectory =
    if pkgs.stdenv.isDarwin
    then "/Users/${username}"
    else "/home/${username}";
  pinentry =
    if pkgs.stdenv.isDarwin
    then pkgs.pinentry_mac
    else pkgs.pinentry-qt;
  pinentry-bin =
    if pkgs.stdenv.isDarwin
    then "${pinentry}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac"
    else "${pinentry}/bin/pinentry";
  pyenvEnable = pkgs.lib.readFile ../sh/pyenv.sh;
  voltaEnable = pkgs.lib.readFile ../sh/volta.sh;
in {
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.05";

  home.sessionVariables = {
    DIRENV_LOG_FORMAT = "";
    NIX_PATH = "nixpkgs=${inputs.nixpkgs}";
    EDITOR = "nvim";
    VISUAL = "nvim";
  };

  home.shellAliases = {
    aws-ecr-login = ''
      aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin "$(aws sts get-caller-identity | jq -r '.Account').dkr.ecr.us-east-1.amazonaws.com"'';
    extract = "${pkgs.atool}/bin/atool --extract --explain --subdir";
    jq = "gojq";
    la = "eza --long --git --time-style=long-iso --all";
    ll = "eza --long --git --time-style=long-iso";
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
            ++ (lib.lists.optionals pkgs.stdenv.isDarwin [
              # Use poll instead of select to get file descriptors.
              (pkgs.fetchpatch {
                url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/8ffe1f83b0521895afd0b48735704af97e2485b0/patches/emacs-29/poll.patch";
                sha256 = "0j26n6yma4n5wh4klikza6bjnzrmz6zihgcsdx36pn3vbfnaqbh5";
              })
              # Enable rounded window with no decoration.
              (pkgs.fetchpatch {
                url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/8ffe1f83b0521895afd0b48735704af97e2485b0/patches/emacs-29/round-undecorated-frame.patch";
                sha256 = "0x187xvjakm2730d1wcqbz2sny07238mabh5d97fah4qal7zhlbl";
              })
              # Make Emacs aware of OS-level light/dark mode.
              (pkgs.fetchpatch {
                url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/8ffe1f83b0521895afd0b48735704af97e2485b0/patches/emacs-28/system-appearance.patch";
                sha256 = "14ndp2fqqc95s70fwhpxq58y8qqj4gzvvffp77snm2xk76c1bvnn";
              })
            ]);
        });
      extraPackages = epkgs:
        with epkgs; [
          ace-window
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
          emms
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
          highlight-indent-guides
          hindent
          hy-mode
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
          minibuffer-line
          minions
          modus-themes
          monky
          nix-mode
          no-littering
          nodejs-repl
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
      enable = personal;
      package =
        if pkgs.stdenv.isDarwin
        then inputs.nixpkgs-firefox.legacyPackages."${system}".firefox-bin
        else pkgs.firefox-bin;
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
      userName = "Joseph LaFreniere";
    };

    home-manager.enable = true;

    htop = {
      enable = true;
      settings.show_program_path = true;
    };

    pandoc.enable = true;

    rbenv = {
      enable = true;
      enableZshIntegration = true;
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
      };
    };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      autocd = true;
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

  home.packages =
    [pinentry]
    ++ (with inputs; [
      gron.packages."${system}".gron
      ripgrep-all.packages."${system}".rga
    ])
    ++ (with pkgs; [
      (aspellWithDicts (aspellDicts: (with aspellDicts; [en en-computers en-science])))
      atool
      aws-sso-creds
      awscli2
      babashka
      cachix
      clang-tools
      clojure
      coreutils
      curl
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
      nixfmt
      nodePackages.bash-language-server
      nodePackages.graphql-language-service-cli
      nodePackages.prettier
      nodePackages.vscode-json-languageserver
      nodePackages.yaml-language-server
      pyright
      ripgrep
      rnix-lsp
      rsync
      rust-analyzer
      subversion
      terraform-ls
      typos
      unrar
      yt-dlp
      zsh
    ])
    ++ (lib.lists.optionals pkgs.stdenv.isLinux (with pkgs; [
      bitwarden-cli
      bitwarden
      dconf2nix
      signal-desktop
      transmission-qt
      ungoogled-chromium
      zoom
    ]))
    ++ (pkgs.lib.lists.optionals (!personal) (with pkgs; [
      groovy
      nodejs
      packer
      terraform
      terraform-docs
      tflint
    ]));

  home.file.".gnupg/gpg-agent.conf".text =
    lib.concatStringsSep "\n"
    (lib.attrsets.mapAttrsToList (name: value: name + " " + toString value) {
      pinentry-program = pinentry-bin;
      default-cache-ttl = 7200;
      default-cache-ttl-ssh = 7200;
    });
}
