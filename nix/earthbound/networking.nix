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

  # Enable Mullvad VPN.
  age.secrets.mullvad.file = ../mullvad.age;
  services.mullvad-vpn.enable = true;
  systemd.services.mullvad-daemon.postStart = let
    mullvad = "${config.services.mullvad-vpn.package}/bin/mullvad";
    secretsFile = config.age.secrets.mullvad.path;
  in ''
    while ! ${mullvad} status >/dev/null; do sleep 1; done
    ${mullvad} auto-connect set on
    ${mullvad} tunnel set ipv6 on
    ${mullvad} lockdown-mode set on
    ${mullvad} relay set location us
    ${mullvad} lan set allow
    ${mullvad} account login $(cat ${secretsFile})'';

  environment.systemPackages = with pkgs; [
    mullvad
    mullvad-vpn
  ];
}
