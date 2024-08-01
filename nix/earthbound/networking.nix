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

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
}
