{
  hostname,
  lib,
  ...
}: {
  networking.hostName = hostname;

  # Enable wireless.
  networking.networkmanager.enable = true;
  networking.wireless.enable = lib.mkDefault false;
  networking.wireguard.enable = true;
  services.resolved.enable = true;
}
