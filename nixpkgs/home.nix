{ config, pkgs, lib, ... }:

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
  };

  home.shellAliases = {
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
    rbenv.enable = true;
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
    gojq
    nixfmt
    nodePackages.prettier
    ripgrep
    rnix-lsp
    rsync
    rust-analyzer
    unrar
    zsh
  ];
}
