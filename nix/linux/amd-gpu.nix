{pkgs, ...}: {
  boot.initrd.kernelModules = ["amdgpu"];
  boot.kernelModules = ["kvm-amd"];

  environment.systemPackages = with pkgs; [
    radeontop
  ];

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      amdvlk
      clinfo
      rocmPackages.clr.icd
      rocmPackages.rocminfo
      rocmPackages.rocm-runtime
    ];
    extraPackages32 = with pkgs.driversi686Linux; [
      amdvlk
    ];
    setLdLibraryPath = true;
  };

  services.foldingathome.enable = true;

  # Heterogeneous-computing Interface for Portability (HIP)
  # https://rocm.docs.amd.com/projects/HIP/en/latest/index.html
  systemd.tmpfiles.rules = let
    rocmEnv = pkgs.symlinkJoin {
      name = "rocm-combined";
      paths = with pkgs.rocmPackages; [
        clr
        hipblas
        rocblas
      ];
    };
  in [
    "L+ /opt/rcom - - - - ${rocmEnv}"
  ];
}
