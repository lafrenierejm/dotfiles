let
  # users
  lafrenierejm = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICBhgW7t1EZ9bk433JOEpazcwvkLtUHkByXk7gdX7lax" # earthbound
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIImNdD6cC3F9ji7xOhnPGs2ABEnxtgx+sG758egL6pgB" # airborn
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBWQAiYw/6KRuf27ApogZq8ds3CB2rSH27PX17Qvoyqx" # darkstar
  ];

  # hosts
  airborn = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG39sKh4+/cK915D2/AFdYefXUL26yUIqy1yUhASyDJv" # root@airborn
  ];
  earthbound = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPlXf9T3ngqpk2cklKUKMdQT5q+0oa+Zf1Kv9FJj2Hjp" # root@earthbound
  ];
  darkstar = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPV4UNRhS2KLKBAXsrtxW77wsgtZ9hkTqD5I8fSXSqE8" # root@darkstar
  ];
in {
  "cachix-auth.age".publicKeys = airborn ++ darkstar ++ earthbound ++ lafrenierejm;
  "earthbound/luks.age".publicKeys = earthbound ++ lafrenierejm;
  "earthbound/tailscale.age".publicKeys = earthbound ++ lafrenierejm;
  "earthbound/zfs-hdd.age".publicKeys = earthbound ++ lafrenierejm;
  "earthbound/earthbound.fin-alioth.ts.net.key.age" = {
    publicKeys = earthbound ++ lafrenierejm;
    armor = true;
  };
  "mullvad.age".publicKeys = earthbound ++ darkstar ++ lafrenierejm;
  "darwin/airborn/b2-key-id.age".publicKeys = airborn ++ lafrenierejm;
  "darwin/airborn/b2-key.age".publicKeys = airborn ++ lafrenierejm;
  "../restic/password.age".publicKeys = airborn ++ earthbound ++ lafrenierejm;
}
