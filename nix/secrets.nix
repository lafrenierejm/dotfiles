let
  # users
  lafrenierejm = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJUCtDj+Vbj5BJF/HncWB03GWyEAoQWczPHGI2DWoWtN" # lafrenierejm@earthbound
  ];

  # hosts
  earthbound = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHJ/z9xZb+vD4nWw1RX/BJplozu1Tq5KT2amfaqAYGas" # root@earthbound
  ];
in {
  "earthbound/luks.age".publicKeys = earthbound ++ lafrenierejm;
}
