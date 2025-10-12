{pkgs, ...}: {
  gtk = {
    enable = true;
    iconTheme = {
      name = "WhiteSur";
      package = pkgs.whitesur-combined;
    };
    theme = {
      name = "WhiteSur";
      package = pkgs.whitesur-gtk-theme;
    };
  };

  qt = {
    enable = true;
    platformTheme = {
      name = "WhiteSur";
      package = pkgs.whitesur-kde;
    };
  };

  home.pointerCursor = {
    name = "WhiteSur";
    package = pkgs.whitesur-combined;
    sway.enable = true;
    gtk.enable = true;
    size = 22;
    x11.enable = true;
  };
}
