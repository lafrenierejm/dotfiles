{pkgs, ...}: {
  fonts = {
    packages = with pkgs; [
      font-awesome
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      noto-fonts-color-emoji
      noto-fonts-lgc-plus
      noto-fonts-monochrome-emoji
      source-code-pro
      source-han-sans
      source-han-serif
    ];
    fontconfig.defaultFonts = {
      serif = [
        "Noto Serif"
        "Source Han Serif"
      ];
      sansSerif = [
        "Noto Sans"
        "Source Han Sans"
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
}
