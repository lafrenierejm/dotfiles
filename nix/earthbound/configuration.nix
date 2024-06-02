# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  inputs,
  config,
  domain,
  pkgs,
  system,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./filesystem.nix
    ./media-server.nix
    ./networking.nix
  ];

  # Use latest kernel that supports ZFS.
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
  boot.kernelParams = ["nohibernate"];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the KDE Plasma Desktop Environment.
  services.displayManager = {
    enable = true;
    defaultSession = "plasma";
    sddm = {
      wayland.enable = true;
      enable = true;
    };
  };
  services.desktopManager.plasma6.enable = true;
  programs.dconf.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.lafrenierejm = {
    isNormalUser = true;
    description = "Joseph LaFreniere";
    extraGroups = ["networkmanager" "wheel"];
    shell = pkgs.zsh;
  };

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
  environment.etc."dual-function-keys.yaml".text = builtins.readFile ../../interception-tools/dual-function-keys.yaml;

  services.udev.packages = with pkgs; [
    teensy-udev-rules
  ];

  # Allow unfree packages
  # nixpkgs.config.permittedInsecurePackages = [
  #   # needed for Folding@home
  #   "python-2.7.18.8"
  #   "python-2.7.18.8-env"
  #   # "electron-24.8.6"
  # ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    clinfo
    curl
    # fahcontrol
    # fahviewer
    neovim
    zsh
    teensy-loader-cli
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      KbdInteractiveAuthentication = false;
      PasswordAuthentication = false;
    };
  };

  # Enable Podman.
  virtualisation.podman = {
    enable = true;
    dockerCompat = true; # create a `docker` alias for podman
    defaultNetwork.settings.dns_enabled = true; # for podman-compose containers to talk to each other
  };

  # services.foldingathome.enable = true;

  # https://nixos.wiki/wiki/Fwupd
  services.fwupd.enable = true;

  networking.hostId = "be1777d9";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
