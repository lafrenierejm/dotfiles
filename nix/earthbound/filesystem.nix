{config, ...}: {
  age.secrets.luks.file = ./luks.age;
  age.secrets.zfs-hdd.file = ./zfs-hdd.age;

  boot.supportedFilesystems = ["ntfs" "zfs"];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/099beb80-1f90-4f04-824d-33fe2fb231c4";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/F282-CE64";
    fsType = "vfat";
  };

  swapDevices = [
    {device = "/dev/disk/by-uuid/dde5d9cf-3ce9-400e-bf1d-8b927596f641";}
  ];

  disko.devices = {
    disk = {
      S0Z4NEAC925823 = {
        type = "disk";
        device = "/dev/disk/by-id/ata-SAMSUNG_SSD_830_Series_S0Z4NEAC925823";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "ssd";
              };
            };
          };
        };
      };
      S0Z4NEAC921334 = {
        type = "disk";
        device = "/dev/disk/by-id/ata-SAMSUNG_SSD_830_Series_S0Z4NEAC921334";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "ssd";
              };
            };
          };
        };
      };
      ZFL317K0 = {
        type = "disk";
        device = "/dev/disk/by-id/ata-ST2000DM008-2FR102_ZFL317K0";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "hdd";
              };
            };
          };
        };
      };
      ZFL31946 = {
        type = "disk";
        device = "/dev/disk/by-id/ata-ST2000DM008-2FR102_ZFL31946";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "hdd";
              };
            };
          };
        };
      };
    };
    zpool = {
      hdd = {
        type = "zpool";
        mode = "mirror";
        rootFsOptions = {
          compression = "zstd";
          "com.sun:auto-snapshot" = "false";
        };
        postCreateHook = "zfs snapshot hdd@blank";

        datasets = {
          media = {
            type = "zfs_fs";
            mountpoint = "/media";
            options."com.sun:auto-snapshot" = "true";
          };
        };
      };
      ssd = {
        type = "zpool";
        mode = "mirror";
        rootFsOptions = {
          compression = "zstd";
          "com.sun:auto-snapshot" = "false";
        };
        postCreateHook = "zfs snapshot ssd@blank";

        datasets = {
          # home = {
          #   type = "zfs_fs";
          #   # mountpoint = "/media";
          #   options."com.sun:auto-snapshot" = "true";
          # };
          # encrypted = {
          #   type = "zfs_fs";
          #   options = {
          #     mountpoint = "none";
          #     encryption = "aes-256-gcm";
          #     keyformat = "passphrase";
          #     keylocation = "file:///tmp/secret.key";
          #   };
          #   # use this to read the key during boot
          #   # postCreateHook = ''
          #   #   zfs set keylocation="prompt" "zroot/$name";
          #   # '';
          # };
          # "encrypted/test" = {
          #   type = "zfs_fs";
          #   mountpoint = "/zfs_crypted";
          # };
        };
      };
    };
  };

  services.udisks2.enable = true;
}
