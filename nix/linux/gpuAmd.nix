{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkOption types;
  cfg = config.services.gpuAmd;
in {
  options.services.gpuAmd = {
    enable = mkOption {
      description = "Services dependent on AMD GPU";
      type = types.bool;
      default = true;
    };
    ports = {
      ollama = mkOption {
        description = "Port for ollama to listen on";
        type = types.port;
        default = 11434;
      };
    };
  };

  config = {
    boot.kernelModules = ["kvm-amd"];

    environment.systemPackages = with pkgs; [
      libva-utils
      lshw
      pciutils
      radeontop
      rocmPackages.rocminfo
    ];

    hardware.amdgpu = {
      initrd.enable = true;
      opencl.enable = true;
    };
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs; [
        rocmPackages.clr.icd
        rocmPackages.rocminfo
        rocmPackages.rocm-runtime
      ];
    };

    services.ollama = {
      enable = true;
      acceleration = "rocm";
      environmentVariables = {
        OLLAMA_HOST = "0.0.0.0:${builtins.toString cfg.ports.ollama}"; # "localhost:${builtins.toString cfg.ports.ollama}";
        OLLAMA_BASE_URL = "https://earthbound.fin-alioth.ts.net/ollama";
        OLLAMA_ORIGINS = "*";
        # OLLAMA_ORIGINS = lib.concatStringsSep "," [
        #   "earthbound.fin-alioth.ts.net"
        #   "localhost"
        # ];
      };
      loadModels = ["qwen2.5-coder:7b"];
      port = cfg.ports.ollama;
      openFirewall = true;
      rocmOverrideGfx = "10.3.0";
    };
    services.nginx.virtualHosts.localhost.locations."/ollama" = {
      proxyPass = "http://localhost:${builtins.toString cfg.ports.ollama}";
      proxyWebsockets = true;
      extraConfig = ''
        #   proxy_pass http://localhost:${builtins.toString cfg.ports.ollama};
        rewrite '^/ollama$' '/' break;
        rewrite '^/ollama/(.*)$' '/$1' break;

        #   # WebSocket support
        #   proxy_http_version 1.1;
        #   proxy_set_header Upgrade $http_upgrade;
        #   proxy_set_header Connection "upgrade";
        #   proxy_read_timeout 60s;
        #   proxy_send_timeout 60s;
        #   proxy_buffering off;

        #   proxy_set_header Host localhost;
        #   proxy_set_header X-Real-IP $remote_addr;
        #   proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        #   proxy_set_header X-Forwarded-Proto $scheme;
        #   proxy_set_header X-Forwarded-Host $host;
        #   proxy_set_header X-Forwarded-Server $host;
      '';
    };

    services.open-webui = {
      enable = true;
      openFirewall = true; # port 8080
      environment = {
        ANONYMIZED_TELEMETRY = "False";
        DO_NOT_TRACK = "True";
        SCARF_NO_ANALYTICS = "True";
        # WEBUI_URL = "http://earthbound.fin-alioth.ts.net/open-webui";
      };
    };
    services.nginx.virtualHosts = {
      open-webui = {
        serverAliases = [
          "earthbound.local"
          "earthbound.fin-alioth.ts.net"
        ];
        listen = [
          {
            inherit (config.services.open-webui) port;
            addr = "earthbound.fin-alioth.ts.net";
          }
        ];
        locations."/" = {
          proxyPass = "http://localhost:${builtins.toString config.services.open-webui.port}$request_uri";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_buffering off;
          '';
        };
      };
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
  };
}
