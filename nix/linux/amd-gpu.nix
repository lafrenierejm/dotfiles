{pkgs, ...}: {
  boot.initrd.kernelModules = ["amdgpu"];
  boot.kernelModules = ["kvm-amd"];

  environment.systemPackages = with pkgs; [
    radeontop
    rocmPackages.rocminfo
  ];

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
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
  };

  services.ollama = {
    enable = true;
    loadModels = ["deepseek-r1:8b" "qwen2.5-coder"];
    acceleration = "rocm";
    rocmOverrideGfx = "10.3.0";
  };
  services.open-webui = {
    enable = true;
    openFirewall = true;
  };

  services.foldingathome.enable = false;

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
