{
  config,
  lib,
  pkgs,
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
    user = mkOption {
      description = "User";
      default = "media";
    };
    group = mkOption {
      description = "Group";
      default = "media";
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
      transmission = mkOption {
        type = types.port;
        default = 9091;
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

    services.transmission = {
      package = pkgs.transmission_4;
      enable = true;
      user = cfg.user;
      group = cfg.group;
      webHome = pkgs.flood-for-transmission;
      downloadDirPermissions = "775";
      settings.trash-original-torrent-files = true;
    };
    systemd.services.transmission.serviceConfig.StateDirectoryMode = "775";
    services.nginx.virtualHosts.localhost.locations = {
      "/transmission" = {
        proxyPass = "http://localhost:${builtins.toString cfg.ports.transmission}";
        proxyWebsockets = true;
      };
    };

    services.netatalk = {
      enable = true;
      port = cfg.ports.netatalk;
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
