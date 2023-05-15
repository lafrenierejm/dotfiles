{ inputs, outputs, lib, config, pkgs, ... }:

{
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
    git
    git-crypt
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
