{
  pkgs,
  userName,
  ...
}: {
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

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd sway";
        user = "greeter";
      };
    };
  };

  security.polkit.enable = true;
  security.pam.services.swaylock = {};

  environment.sessionVariables.NIXOS_OZONE_WL = "1";
  environment.systemPackages = with pkgs; [
    grim
    mako
    slurp
    sway
    wl-clipboard
  ];

  # Enable the gnome-keyring secrets vault.
  # Will be exposed through DBus to programs willing to store secrets.
  services.gnome.gnome-keyring.enable = true;

  # Enable geoclue for location services
  services.geoclue2 = {
    enable = true;
    appConfig.darkman = {
      isAllowed = true;
      isSystem = false;
      users = [userName];
    };
  };

  users.users.greeter = {};
}
