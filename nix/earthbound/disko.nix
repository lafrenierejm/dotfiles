{config, ...}: {
  disko.devices = {
    disk = {
      # /media
      sda = {
        type = "disk";
        device = "/dev/sda";
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
      sdd = {
        type = "disk";
        device = "/dev/sdd";
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
      # /
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
