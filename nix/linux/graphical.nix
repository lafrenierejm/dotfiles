{pkgs, ...}: rec {
  security.polkit.enable = true;
  programs.dconf.enable = true;

  services.kanshi = {
    enable = true;

    profiles = {
      home_office = {
        outputs = [
          {
            criteria = "DP-1";
            scale = 2;
            status = "enable";
          }
          {
            criteria = "DP-2";
            scale = 2;
            status = "enable";
          }
          {
            criteria = "DP-3";
            scale = 2;
            status = "enable";
          }
        ];
      };
    };
  };

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd sway";
        user = "greeter";
      };
    };
  };

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  environment.systemPackages = with pkgs; [
    grim
    slurp
    wl-clipboard
    mako
  ];

  services.gnome.gnome-keyring.enable = true;

  users.users.greeter = {};
}
