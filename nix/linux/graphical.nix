{pkgs, ...}: rec {
  security.polkit.enable = true;
  programs.dconf.enable = true;

  environment.etc."kanshi.sh".text =
    # give sway a little time to startup before starting kanshi.
    "exec sleep 5; systemctl --user start kanshi.service";
  systemd.user.services.kanshi = {
    description = "kanshi daemon";
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.kanshi}/bin/kanshi -c ${environment.etc."kanshi.sh"}";
    };
  };

  security.pam.services.swaylock = {};

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd sway";
        user = "greeter";
      };
    };
  };

  users.users.greeter = {};
}
