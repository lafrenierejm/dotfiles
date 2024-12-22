{...}: {
  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [
    548 # netatalk
    4700 # CNID
  ];
  # networking.firewall.allowedUDPPorts = [ ... ];

  services.netatalk = {
    enable = true;
    port = 548; # default
    settings = {
      Global = {
        "admin auth user" = "root";
        "uam list" = "uams_dhx2_pam.so";
      };
      Homes = {
        "basedir regex" = "/home";
      };
    };
  };
}
