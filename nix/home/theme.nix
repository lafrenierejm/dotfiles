{
  pkgs,
  lib,
  ...
}: {
  gtk = {
    enable = true;
    iconTheme = {
      name = "WhiteSur";
      package = pkgs.whitesur-combined;
    };
    theme = {
      name = "WhiteSur-Dark";
      package = pkgs.whitesur-gtk-theme;
    };
  };

  qt = {
    enable = true;
    platformTheme = {
      name = "WhiteSur-Dark";
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

  # Install darkman for automatic theme switching
  services.darkman = {
    enable = true;
    settings = {
      lat = 30.27;
      lng = -97.74;
    };
    lightModeScripts = {
      gtk-theme = ''
        ${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/gtk-theme "'WhiteSur-Light'"
        ${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/color-scheme "'prefer-light'"
      '';
      qt-theme = ''
        ${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/icon-theme "'WhiteSur'"
      '';
      emacs = ''
        # Switch Emacs theme to light mode
        ${pkgs.emacs}/bin/emacsclient --eval "(lafrenierejm/apply-theme 'light)" || true
      '';
      waybar = ''
        # Restart waybar to pick up the new theme
        ${pkgs.systemd}/bin/systemctl --user restart waybar || true
      '';
      notify = ''
        ${pkgs.libnotify}/bin/notify-send -i weather-clear "darkman" "Switched to light mode"
      '';
    };
    darkModeScripts = {
      gtk-theme = ''
        ${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/gtk-theme "'WhiteSur-Dark'"
        ${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"
      '';
      qt-theme = ''
        ${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/icon-theme "'WhiteSur'"
      '';
      emacs = ''
        # Switch Emacs theme to dark mode
        ${pkgs.emacs}/bin/emacsclient --eval "(lafrenierejm/apply-theme 'dark)" || true
      '';
      waybar = ''
        # Restart waybar to pick up the new theme
        ${pkgs.systemd}/bin/systemctl --user restart waybar || true
      '';
      notify = ''
        ${pkgs.libnotify}/bin/notify-send -i weather-clear-night "darkman" "Switched to dark mode"
      '';
    };
  };

  # Install both light and dark themes
  home.packages = with pkgs; [
    dconf
    libnotify
  ];
}
