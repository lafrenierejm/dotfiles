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
      "--exit-node-allow-lan-access"
      "--accept-dns=true"
    ];
  };
  environment.systemPackages = with pkgs; [
    tailscale
    tailscale-systray
    tailscalesd
  ];
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22 # SSH
    ];
    allowedUDPPorts = [config.services.tailscale.port];
    trustedInterfaces = ["tailscale0"];
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
}
