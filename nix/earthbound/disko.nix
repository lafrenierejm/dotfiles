{config, ...}: {
  age.secrets.luks.file = ./luks.age;
  age.secrets.zfs-hdd.file = ./zfs-hdd.age;
  disko.devices = {
    disk = {
      # "/dev/disk/by-id/ata-SAMSUNG_SSD_830_Series_S0Z4NEAC925823"
      ata-SAMSUNG_SSD_830_Series_S0Z4NEAC925823 = {
        type = "disk";
        device = "/dev/disk/by-id/ata-SAMSUNG_SSD_830_Series_S0Z4NEAC925823";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "zroot";
              };
            };
          };
        };
      };
      # sdb
      ata-ST2000DM008-2FR102_ZFL317K0 = {
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
      # sdc
      wwn-0x5002538043584d30 = {
        type = "disk";
        device = "/dev/disk/by-id/wwn-0x5002538043584d30";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "zroot";
              };
            };
          };
        };
      };
      # sdd
      wwn-0x5000c500c793e7fd = {
        type = "disk";
        device = "/dev/disk/by-id/wwn-0x5000c500c793e7fd";
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
      nvme0n1 = {
        type = "disk";
        device = "/dev/nvme0n1";
        content = {
          type = "gpt";
          partitions = {
            esp = {
              size = "512M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            luks = {
              size = "100%";
              content = {
                type = "luks";
                name = "crypted";
                extraOpenArgs = ["--allow-discards"];
                passwordFile = config.age.secrets.luks.path;
                content = {
                  type = "filesystem";
                  format = "ext4";
                  mountpoint = "/";
                };
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
          # media = {
          #   type = "zfs_fs";
          #   mountpoint = "/media";
          #   options."com.sun:auto-snapshot" = "true";
          # };
          encrypted = {
            type = "zfs_fs";
            options = {
              mountpoint = "none";
              encryption = "aes-256-gcm";
              keyformat = "passphrase";
              keylocation = "${config.age.secrets.zfs-hdd.file}";
            };
            # Read the encryption key during boot.
            # postCreateHook = ''
            #   zfs set keylocation="prompt" "hdd/$name";
            # '';
          };
          "encrypted/media" = {
            type = "zfs_fs";
            mountpoint = "/media_2";
          };
        };
      };
      zroot = {
        type = "zpool";
        mode = "mirror";
        rootFsOptions = {
          compression = "zstd";
          "com.sun:auto-snapshot" = "false";
        };
        postCreateHook = "zfs snapshot zroot@blank";

        datasets = {
          media = {
            type = "zfs_fs";
            mountpoint = "/media";
            options."com.sun:auto-snapshot" = "true";
          };
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
}
