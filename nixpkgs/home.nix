{ inputs, lib, config, pkgs, system, username, gitEmail, gitUseGpg, ... }:

let
  homeDirectory =
    if pkgs.stdenv.isDarwin then "/Users/${username}" else "/home/${username}";
  pinentry = if pkgs.stdenv.isDarwin then pkgs.pinentry_mac else pkgs.pinentry;
  pinentry-bin = if pkgs.stdenv.isDarwin then
    "${pinentry}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac"
  else
    "${pinentry}/bin/pinentry";
in {
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  home.sessionVariables = {
    DIRENV_LOG_FORMAT = "";
    NIX_PATH = "nixpkgs=${inputs.nixpkgs}";
  };

  home.shellAliases = {
    aws-ecr-login = ''
      aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin "$(aws sts get-caller-identity | jq -r '.Account').dkr.ecr.us-east-1.amazonaws.com"'';
    extract = "${pkgs.atool}/bin/atool --extract --explain --subdir";
    jq = "gojq";
    la = "exa --long --git --time-style=long-iso --all";
    ll = "exa --long --git --time-style=long-iso";
  };

  programs = {
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
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
        alias =
          "! git config --get-regexp '^alias.' | sed -e 's/^alias.//' -e 's/ / = /'";
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
        push = { gpgSign = if gitUseGpg then "if-asked" else "false"; };
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
      signing = if gitUseGpg then {
        key = "0375DD9AEDD168A3ADA39EBAEE236AA0141EFCA3";
        signByDefault = true;
      } else
        null;
      userEmail = gitEmail;
      userName = "Joseph LaFreniere";
    };
    home-manager.enable = true;
    htop = {
      enable = true;
      settings.show_program_path = true;
    };
    rbenv = {
      enable = true;
      enableZshIntegration = true;
    };
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableSyntaxHighlighting = true;
      autocd = true;
      initExtra = lib.concatStringsSep "\n" [
        ''. "$HOME/.config/zsh/vterm.zsh"''
        # pyenv
        ''export PYENV_ROOT="$HOME/.pyenv"''
        ''command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"''
        ''eval "$(pyenv init -)"''
      ];
      profileExtra = lib.concatStringsSep "\n"
        [ ''export PATH="$PATH:$HOME/.dotnet/tools"'' ];
    };
  };

  home.packages = [ pinentry ] ++ (with inputs; [
    ripgrep-all.packages."${system}".rga
    ripsecrets.packages."${system}".ripsecrets
  ]) ++ (with pkgs; [
    aspell
    aspellDicts.en
    atool
    aws-sso-creds
    awscli2
    babashka
    bitwarden-cli
    cachix
    clojure
    coreutils
    curl
    exa
    fd
    ghq
    git-crypt
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
    nodePackages.prettier
    pyright
    ripgrep
    rnix-lsp
    rsync
    rust-analyzer
    terraform-ls
    unrar
    youtube-dl
    zsh
  ]);

  home.file.".gnupg/gpg-agent.conf".text = lib.concatStringsSep "\n"
    (lib.attrsets.mapAttrsToList (name: value: name + " " + toString value) {
      pinentry-program = pinentry-bin;
      default-cache-ttl = 7200;
      default-cache-ttl-ssh = 7200;
    });
}
