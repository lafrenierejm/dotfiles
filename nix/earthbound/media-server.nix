{
  domain,
  pkgs,
  ...
}: let
  user = "media";
in {
  users = {
    groups.media.gid = 993;
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
      "jellyfin.${domain}" = {
        locations."/" = {
          proxyPass = "http://localhost:8096";
        };
      };
    };
  };
}
