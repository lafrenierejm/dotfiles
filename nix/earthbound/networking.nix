{
  config,
  pkgs,
  ...
}: {
  # Enable networking
  networking.hostName = "earthbound"; # Define your hostname.

  # Enable wireless.
  networking.networkmanager.enable = true;
  networking.wireless.enable = false;
  networking.wireguard.enable = true;
  services.resolved.enable = true;

  # Enable Tailscale.
  age.secrets.tailscale.file = ./tailscale.age;
  services.tailscale = {
    enable = true;
    openFirewall = true;
    useRoutingFeatures = "client";
    authKeyFile = config.age.secrets.tailscale.path;
    extraUpFlags = [
      "--exit-node=us-mia-wg-003.mullvad.ts.net"
      "--exit-node-allow-lan-access=true"
      "--accept-dns=true"
    ];
  };
  environment.systemPackages = with pkgs; [
    tailscale
    tailscale-systray
    tailscalesd
  ];
  networking.firewall.trustedInterfaces = ["tailscale0"];

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
  age.secrets.sslCrt = {
    file = ./ssl.crt.age;
    owner = "nginx";
    group = "nginx";
    mode = "440";
  };
  age.secrets.sslKey = {
    file = ./ssl.key.age;
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
            addr = "earthbound.fin-alioth.ts.net";
            port = 80;
          }
          {
            addr = "earthbound.fin-alioth.ts.net";
            port = 443;
            ssl = true;
          }
        ];
        sslCertificate = config.age.secrets.sslCrt.path;
        sslCertificateKey = config.age.secrets.sslKey.path;
      };
    };
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
}
