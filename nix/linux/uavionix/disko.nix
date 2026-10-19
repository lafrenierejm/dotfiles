{
  # NixOS is the only OS on this device; it is wiped when disko runs.
  disko.devices.disk.main = {
    type = "disk";
    device = "/dev/nvme0n1";
    content = {
      type = "gpt";
      partitions = {
        # The ESP cannot be encrypted: the firmware must read the bootloader,
        # kernel, and initrd from it before the LUKS volume can be unlocked.
        ESP = {
          size = "1G";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = ["fmask=0077" "dmask=0077"];
          };
        };
        # Everything else, including swap, lives inside one LUKS2 container so
        # that a single passphrase unlocks the whole system.
        luks = {
          size = "100%";
          content = {
            type = "luks";
            name = "cryptroot";
            settings.allowDiscards = true; # TRIM for SSD longevity/performance
            content = {
              type = "btrfs";
              extraArgs = ["-L" "nixos"];
              subvolumes = {
                "/root" = {
                  mountpoint = "/";
                  mountOptions = ["compress=zstd"];
                };
                "/home" = {
                  mountpoint = "/home";
                  mountOptions = ["compress=zstd"];
                };
                "/nix" = {
                  mountpoint = "/nix";
                  mountOptions = [
                    "compress=zstd"
                    "noatime" # disregard access times to reduce disk wear
                  ];
                };
                "/swap" = {
                  mountpoint = "/.swapvol";
                  swap.swapfile.size = "40G"; # 32768 MB of physical RAM + 8 GB headroom
                };
              };
            };
          };
        };
      };
    };
  };
}
