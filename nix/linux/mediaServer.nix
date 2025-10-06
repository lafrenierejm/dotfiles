{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkOption types;
  cfg = config.services.mediaServer;
  user = "media";
in {
  options.services.mediaServer = {
    enable = mkOption {
      description = "Serve media";
      type = types.bool;
      default = true;
    };
    ports = {
      lidarr = mkOption {
        type = types.port;
        default = 8686;
      };
      jellyfin = mkOption {
        type = types.port;
        default = 8096;
      };
    };
  };

  config = {
    users = {
      groups.${user}.gid = 993;
      users.${user} = {
        isSystemUser = true;
        extraGroups = [
          "render"
          "video"
        ];
        group = "media";
        uid = 993;
      };
    };

    # Jellyfin runs on port 8096.
    services.jellyfin = {
      enable = true;
      user = user;
    };
    environment.systemPackages = with pkgs; [
      (jellyfin-ffmpeg.override {
        ffmpeg_7-full = ffmpeg_7-full.override {
          withUnfree = true;
          withOpengl = true;
        };
      })
      jellyfin-web
    ];

    services.lidarr = {
      enable = true;
      user = user;
      group = user;
      settings = {
        server = {
          urlbase = "localhost";
          port = cfg.ports.lidarr;
          bindaddress = "*";
        };
      };
    };

    # Open ports in the firewall.
    networking.firewall.allowedTCPPorts = [80 443];
    # networking.firewall.allowedUDPPorts = [ ... ];

    services.avahi = {
      enable = true;
      publish = {
        enable = true;
        addresses = true;
        workstation = true;
      };
    };

    services.nginx = {
      enable = true;

      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = false;

      virtualHosts = {
        localhost = {
          serverAliases = [
            "localhost"
            "earthbound.local"
            "earthbound.fin-alioth.ts.net"
            "www.earthbound.fin-alioth.ts.net"
          ];
          listen = [
            {
              addr = "localhost";
            }
            {
              addr = "earthbound.fin-alioth.ts.net";
            }
          ];
          locations =
            lib.mapAttrs'
            (service: port:
              lib.nameValuePair
              "/${service}"
              {proxyPass = "http://localhost:${builtins.toString port}";})
            cfg.ports;
        };
      };
    };
  };
}
