{
  config,
  lib,
  ...
}: let
  inherit (lib) mkOption types;
  cfg = config.services.fileServer;
in {
  options.services.fileServer = {
    enable = mkOption {
      description = "Serve files";
      type = types.bool;
      default = true;
    };
    ports = {
      cnid = mkOption {
        type = types.port;
        default = 4700;
      };
      netatalk = mkOption {
        type = types.port;
        default = 548;
      };
    };
  };

  config = {
    # Open ports in the firewall.
    networking.firewall.allowedTCPPorts = [
      cfg.ports.cnid
      cfg.ports.netatalk
    ];
    # networking.firewall.allowedUDPPorts = [ ... ];

    services.netatalk = {
      enable = true;
      port = 548; # default
      settings = {
        Global = {
          "admin auth user" = "root";
          "uam list" = "uams_dhx2_pam.so";
        };
        Homes = {
          "basedir regex" = "/home";
        };
      };
    };
  };
}
