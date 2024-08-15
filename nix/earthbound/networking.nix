{
  config,
  pkgs,
  ...
}: {
  # Enable networking
  networking.hostName = "earthbound"; # Define your hostname.
  networking.networkmanager.enable = true;
  networking.wireless.enable = false;
  networking.wireguard.enable = true;
  services.resolved.enable = true;

  # Enable Tailscale.
  age.secrets.tailscale.file = ./tailscale.age;
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "client";
  };
  environment.systemPackages = with pkgs; [
    tailscale
    tailscale-systray
    tailscalesd
  ];
  ## The below service definition is copyright (c) Tailscale.
  ## Its original source is https://tailscale.com/kb/1096/nixos-minecraft.
  systemd.services.tailscale-autoconnect = {
    description = "Automatically connect to Tailscale";

    # Ensure the tailscale service is running before attempting to connect.
    after = ["network-pre.target" "tailscale.service"];
    wants = ["network-pre.target" "tailscale.service"];
    wantedBy = ["multi-user.target"];

    serviceConfig.Type = "oneshot";

    script = ''
      # Wait for tailscaled to settle.
      sleep 2

      # If we're already authenticated, then do nothing.
      status="$(${pkgs.tailscale}/bin/tailscale status -json | ${pkgs.jq}/bin/jq -r .BackendState)"
      if [ $status = "Running" ]; then
        exit 0
      fi

      # Otherwise, authenticate with tailscale.
      "${pkgs.tailscale}/bin/tailscale" up --exit-node=us-mia-wg-003.mullvad.ts.net --exit-node-allow-lan-access --auth-key="file:${config.age.secrets.tailscale.path}"
    '';
  };

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
