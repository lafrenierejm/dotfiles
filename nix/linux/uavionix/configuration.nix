# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  hostname,
  ...
}: {
  imports = [
    ../audio.nix
    ../graphical.nix
    ../networking.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 25;
  boot.loader.efi.canTouchEfiVariables = true;

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Hibernation resumes from the swapfile in the encrypted btrfs volume (see
  # ./disko.nix). The offset only exists once the swapfile has been created;
  # after installing, set it to the output of
  #   btrfs inspect-internal map-swapfile -r /.swapvol/swapfile
  boot.resumeDevice = "/dev/mapper/cryptroot";
  boot.kernelParams = let
    resumeOffset = 533760; # `btrfs inspect-internal map-swapfile -r /.swapvol/swapfile`
  in
    pkgs.lib.optional (resumeOffset != null) "resume_offset=${toString resumeOffset}";

  # Enable networking
  networking.networkmanager.enable = true;

  services.interception-tools = {
    enable = true;
    plugins = [pkgs.interception-tools-plugins.dual-function-keys];
    udevmonConfig = builtins.toJSON [
      {
        JOB = "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools-plugins.dual-function-keys}/bin/dual-function-keys -c /etc/dual-function-keys.yaml | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE";
        DEVICE.EVENTS.EV_KEY = [
          "KEY_LEFTCTRL"
          "KEY_SPACE"
        ];
      }
    ];
  };
  environment.etc."dual-function-keys.yaml".text = builtins.readFile ../../../interception-tools/dual-function-keys.yaml;

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      KbdInteractiveAuthentication = false;
      PasswordAuthentication = false;
    };
  };
  programs.mosh.enable = true;

  # Enable Podman.
  virtualisation.containers.enable = true;
  virtualisation.podman = {
    enable = true;
    dockerCompat = true; # create a `docker` alias for podman
    defaultNetwork.settings.dns_enabled = true; # for podman-compose containers to talk to each other
    autoPrune.enable = true;
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  programs.steam.enable = true;

  services.fwupd.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?
}
