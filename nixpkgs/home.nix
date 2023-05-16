{ inputs, outputs, lib, config, pkgs, username, gitEmail, gitUseGpg, ... }:

let
  homeDirectory =
    if pkgs.stdenv.isLinux then "/home/${username}" else "/Users/${username}";
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
      ignores = [
        # emacs
        "*#"
        "*.elc"
        "*~"
        ".dir-locals.el"
        # direnv
        ".direnv/"
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
      enableVteIntegration = true;
      autocd = true;
      initExtra = ". ~/.config/zsh/prompt.zsh";
    };
  };

  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    atool
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
  ];
}
