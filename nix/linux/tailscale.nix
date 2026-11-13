{
  config,
  lib,
  pkgs,
  personal,
  ...
}: {
  services.tailscale = {
    enable = true;
    openFirewall = true;
    useRoutingFeatures = "client";
    # Only authenticate automatically on hosts that declare the secret.
    authKeyFile = lib.mkIf (config.age.secrets ? tailscale) config.age.secrets.tailscale.path;
    extraUpFlags = lib.lists.flatten [
      "--exit-node-allow-lan-access=true"
      "--accept-dns=true"
      (lib.lists.optionals personal [
        "--exit-node=us-mia-wg-003.mullvad.ts.net"
      ])
    ];
  };
  environment.systemPackages = with pkgs; [
    tailscale
    tailscale-systray
    tailscalesd
  ];
  networking.firewall.trustedInterfaces = ["tailscale0"];
}
