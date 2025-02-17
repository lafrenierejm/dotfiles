{pkgs, ...}: {
  catppuccin = {
    enable = true;
    flavor = "mocha";
    accent = "lavender";

    cursors.enable = true;
    gtk.enable = true;
    kvantum.enable = true;
  };

  gtk.enable = true;

  qt = {
    enable = true;
    platformTheme.name = "kvantum";
    style.name = "kvantum";
  };

  home.packages = with pkgs; [
    catppuccin-kvantum
    catppuccin-papirus-folders
    libsForQt5.qt5ct
  ];

  home.pointerCursor = {
    gtk.enable = true;
    size = 24;
    x11.enable = true;
  };
}
