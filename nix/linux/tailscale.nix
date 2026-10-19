{
  config,
  lib,
  pkgs,
  ...
}: {
  services.tailscale = {
    enable = true;
    openFirewall = true;
    useRoutingFeatures = "client";
    # Only authenticate automatically on hosts that declare the secret.
    authKeyFile = lib.mkIf (config.age.secrets ? tailscale) config.age.secrets.tailscale.path;
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
