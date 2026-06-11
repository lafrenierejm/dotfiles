{
  config,
  pkgs,
  lib,
  ...
}: {
  # Firewall
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22 # SSH
      80 # HTTP
      443 # HTTPS
    ];
    allowedUDPPorts = [
      22 # SSH
      80 # QUIC
      443 # QUIC
      config.services.tailscale.port
    ];
  };

  # NGINX
  age.secrets.sslKey = {
    file = ./earthbound.fin-alioth.ts.net.key.age;
    owner = "nginx";
    group = "nginx";
    mode = "440";
  };
  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    virtualHosts = {
      localhost = {
        addSSL = true;
        serverAliases = [
          "localhost"
          "earthbound.local"
          "earthbound.fin-alioth.ts.net"
          "www.earthbound.fin-alioth.ts.net"
        ];
        listen = [
          {
            addr = "localhost";
            port = 80;
          }
          {
            addr = "localhost";
            port = 443;
            ssl = true;
          }
          {
            addr = "0.0.0.0";
            port = 80;
          }
          {
            addr = "0.0.0.0";
            port = 443;
            ssl = true;
          }
        ];
        sslCertificate = ./earthbound.fin-alioth.ts.net.crt;
        sslCertificateKey = config.age.secrets.sslKey.path;
      };
    };
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
}
