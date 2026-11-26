{
  config,
  lib,
  pkgs,
  personal,
  userName,
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
    # Let this user run `tailscale` commands without sudo.
    extraSetFlags = ["--operator=${userName}"];
  };
  environment.systemPackages = with pkgs; [
    tailscale
    tailscalesd
  ];
  networking.firewall.trustedInterfaces = ["tailscale0"];
}
