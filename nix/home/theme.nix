{pkgs, ...}: {
  gtk = {
    enable = true;
    iconTheme = {
      name = "elementary";
      package = pkgs.pantheon.elementary-icon-theme;
    };
  };

  qt = {
    enable = true;
    platformTheme.name = "kvantum";
    style.name = "kvantum";
  };

  home.pointerCursor = {
    name = "macOS";
    package = pkgs.apple-cursor;
    sway.enable = true;
    gtk.enable = true;
    size = 24;
    x11.enable = true;
  };
}
