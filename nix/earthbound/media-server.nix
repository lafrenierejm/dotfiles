{
  domain,
  pkgs,
  ...
}: {
  # Jellyfin runs on port 8096.
  services.jellyfin.enable = true;

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
