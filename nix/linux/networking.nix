{
  config,
  hostname,
  lib,
  pkgs,
  ...
}: {
  networking.hostName = hostname;

  # Enable wireless.
  networking.networkmanager.enable = true;
  networking.wireless.enable = lib.mkDefault false;
  networking.wireguard.enable = true;
  services.resolved.enable = true;

  # Enable Tailscale.
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
}
