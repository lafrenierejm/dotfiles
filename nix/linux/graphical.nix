{
  lib,
  pkgs,
  ...
}: {
  fonts = {
    packages = with pkgs; [
      dejavu_fonts
      font-awesome
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      noto-fonts-color-emoji
      noto-fonts-lgc-plus
      noto-fonts-monochrome-emoji
      source-code-pro
      source-sans
      source-serif
      source-han-sans
      source-han-serif
    ];
    fontconfig.defaultFonts = {
      serif = [
        "Source Serif"
        "Source Han Serif"
        "Noto Serif"
      ];
      sansSerif = [
        "Source Sans"
        "Source Han Sans"
        "Noto Sans"
      ];
    };
    fontDir.enable = true;
  };

  programs.dconf.enable = true;

  security.polkit.enable = true;

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # Enable the gnome-keyring secrets vault.
  # Will be exposed through DBus to programs willing to store secrets.
  services.gnome.gnome-keyring.enable = true;
  # The gnome-keyring module only hooks PAM for `login`;
  # unlock the keyring on COSMIC greeter and lock-screen authentication too.
  security.pam.services.cosmic-greeter.enableGnomeKeyring = true;

  # Geoclue disabled - darkman now uses static location
  # services.geoclue2 = {
  #   enable = true;
  #   appConfig.darkman = {
  #     isAllowed = true;
  #     isSystem = false;
  #     users = [(toString config.users.users.${userName}.uid)];
  #   };
  # };

  users.users.greeter = {};

  services.displayManager.cosmic-greeter.enable = true;
  services.desktopManager.cosmic.enable = true;
  services.system76-scheduler.enable = true;
  environment.cosmic.excludePackages = with pkgs; [
    cosmic-edit
    cosmic-term
  ];
  environment.systemPackages = with pkgs; [
    whitesur-combined
    whitesur-gtk-theme
    whitesur-kde
    trayscale
  ];

  # Start Trayscale with the desktop session, tray only (no window).
  # COSMIC's status area applet displays the StatusNotifierItem it registers.
  # Trayscale needs the Tailscale operator (see tailscale.nix) to control the daemon.
  environment.etc."xdg/autostart/trayscale.desktop".text = ''
    [Desktop Entry]
    Type=Application
    Name=Trayscale
    Exec=${lib.getExe pkgs.trayscale} --hide-window
  '';
}
