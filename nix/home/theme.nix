{
  config,
  pkgs,
  lib,
  ...
}: {
  catppuccin = {
    enable = true;
    flavor = "mocha";
    accent = "lavender";
    gtk.enable = true;
  };

  gtk.enable = true;

  qt = {
    enable = true;
    platformTheme.name = "kvantum";
    style = {
      name = "kvantum";
      catppuccin = {
        enable = true;
        apply = true;
      };
    };
  };

  home.packages = with pkgs; [
    catppuccin-kvantum
    catppuccin-papirus-folders
    libsForQt5.qt5ct
  ];

  home.pointerCursor = {
    name = "catppuccin-latte-light-cursors";
    gtk.enable = true;
    package = pkgs.catppuccin-cursors.latteLight;
    size = 24;
    # x11 = {
    #   enable = true;
    #   defaultCursor = "Breeze";
    # };
  };
}
