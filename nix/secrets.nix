let
  # users
  lafrenierejm = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICBhgW7t1EZ9bk433JOEpazcwvkLtUHkByXk7gdX7lax" # lafrenierejm@earthbound
  ];

  # hosts
  earthbound = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPlXf9T3ngqpk2cklKUKMdQT5q+0oa+Zf1Kv9FJj2Hjp" # root@earthbound
  ];
in {
  "earthbound/luks.age".publicKeys = earthbound ++ lafrenierejm;
  "earthbound/tailscale.age".publicKeys = earthbound ++ lafrenierejm;
  "earthbound/zfs-hdd.age".publicKeys = earthbound ++ lafrenierejm;
  "earthbound/ssl.crt.age" = {
    publicKeys = earthbound ++ lafrenierejm;
    armor = true;
  };
  "earthbound/ssl.key.age" = {
    publicKeys = earthbound ++ lafrenierejm;
    armor = true;
  };
  "mullvad.age".publicKeys = earthbound ++ lafrenierejm;
}
