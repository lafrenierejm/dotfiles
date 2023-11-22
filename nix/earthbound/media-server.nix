{
  domain,
  pkgs,
  ...
}: {
  services.transmission = {
    enable = true;
    user = "media";
    group = "media";
    settings = {
      download-dir = "/media/complete";
      incomplete-dir = "/media/incomplete";
      watch-dir-enabled = false;
    };
  };
  environment.systemPackages = [pkgs.transmission-qt];

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
    recommendedTlsSettings = true;

    virtualHosts = {
      "jellyfin.${domain}" = {
        enableACME = true;
        locations."/" = {
          proxyPass = "http://localhost:8096";
        };
        onlySSL = true;
      };
    };

    # Only allow PFS-enabled ciphers with AES256
    sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";

    appendHttpConfig = ''
      # Add HSTS header with preloading to HTTPS requests.
      # Adding this header to HTTP requests is discouraged
      map $scheme $hsts_header {
          https   "max-age=31536000; includeSubdomains; preload";
      }
      add_header Strict-Transport-Security $hsts_header;

      # Enable CSP for your services.
      #add_header Content-Security-Policy "script-src 'self'; object-src 'none'; base-uri 'none';" always;

      # Minimize information leaked to other domains
      add_header 'Referrer-Policy' 'origin-when-cross-origin';

      # Disable embedding as a frame
      add_header X-Frame-Options DENY;

      # Prevent injection of code in other mime types (XSS Attacks)
      add_header X-Content-Type-Options nosniff;

      # This might create errors
      proxy_cookie_path / "/; secure; HttpOnly; SameSite=strict";
    '';
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "acme@${domain}";
  };
}
