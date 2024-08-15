let
  # users
  lafrenierejm = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJUCtDj+Vbj5BJF/HncWB03GWyEAoQWczPHGI2DWoWtN" # lafrenierejm@earthbound
  ];

  # hosts
  earthbound = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPlXf9T3ngqpk2cklKUKMdQT5q+0oa+Zf1Kv9FJj2Hjp" # root@earthbound
  ];
in {
  "earthbound/luks.age".publicKeys = earthbound ++ lafrenierejm;
  "earthbound/tailscale.age".publicKeys = earthbound ++ lafrenierejm;
  "earthbound/zfs-hdd.age".publicKeys = earthbound ++ lafrenierejm;
  "mullvad.age".publicKeys = earthbound ++ lafrenierejm;
}
