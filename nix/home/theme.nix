{pkgs, ...}: {
  home.packages = with pkgs; [
    whitesur-icon-theme
    whitesur-gtk-theme
    whitesur-kde
  ];

  gtk = {
    enable = true;
    iconTheme = {
      name = "WhiteSur";
      package = pkgs.whitesur-icon-theme;
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
    package = pkgs.whitesur-icon-theme;
    sway.enable = true;
    gtk.enable = true;
    size = 24;
    x11.enable = true;
  };

  xdg.dataFile."icons/WhiteSur".source = "${pkgs.whitesur-icon-theme}/share/icons/WhiteSur";
  xdg.dataFile."icons/WhiteSur-cursors".source = "${pkgs.whitesur-cursors}/share/icons/WhiteSur-cursors";
  # xdg.dataFile."icons/macOS".source = "${pkgs.apple-cursor}/share/icons/macOS";
}
