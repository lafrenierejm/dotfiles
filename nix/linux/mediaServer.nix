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
          urlbase = "lidarr";
          port = 8686;
          bindaddress = "*";
        };
      };
    };

    services.radarr = {
      enable = true;
      user = user;
      group = user;
      settings = {
        server = {
          urlbase = "radarr";
          port = 7878;
          bindaddress = "*";
        };
      };
    };

    services.sonarr = {
      enable = true;
      user = user;
      group = user;
      settings = {
        server = {
          urlbase = "sonarr";
          port = 8989;
          bindaddress = "*";
        };
      };
    };

    services.avahi = {
      enable = true;
      publish = {
        enable = true;
        addresses = true;
        workstation = true;
      };
    };

    services.nginx.virtualHosts.localhost.locations =
      lib.mapAttrs'
      (
        service: port:
          lib.nameValuePair
          "/${service}"
          {
            proxyPass = "http://localhost:${builtins.toString port}";
            proxyWebsockets = true;
          }
      )
      {
        jellyfin = 8096;
        lidarr = config.services.lidarr.settings.server.port;
        radarr = config.services.radarr.settings.server.port;
        sonarr = config.services.sonarr.settings.server.port;
      };
  };
}
