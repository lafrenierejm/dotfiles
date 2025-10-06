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
  };

  config = {
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

    services.nginx.virtualHosts."localhost".locations = {
      "/open-webui/html/static/" = {
        alias = "${pkgs.open-webui}/lib/python3.12/site-packages/open_webui/static/";
        # autoindex = "off";
      };
      "/open-webui" = {
        proxyPass = "http://localhost:${builtins.toString config.services.open-webui.port}";
        proxyWebsockets = true;
        root = "${pkgs.open-webui}/lib/python3.12/site-packages/open_webui/static"; # config.services.open-webui.stateDir;
        extraConfig = ''
          rewrite "^/open-webui/(.*)$" "/$1" break;

          sub_filter_types text/plain text/html application/javascript;
          sub_filter 'href="/' 'href="/open-webui/';
          sub_filter 'src="/' 'src="/open-webui/';
          sub_filter '/_app' '/open-webui/_app';
          sub_filter '/api' '/open-webui/api';
          sub_filter '/favicon.png' '/open-webui/html/static/favicon.png';
          sub_filter '/ollama' '/open-webui/ollama';
          sub_filter '/openai' '/open-webui/openai';
          sub_filter '/static' '/open-webui/static';
          sub_filter '/themes' '/open-webui/themes';
          sub_filter '/user.png' '/open-webui/user.png';
          sub_filter_once off;

          proxy_buffering off;
        '';
      };
      "/ollama" = {
        proxyPass = "http://$host:11434";
        proxyWebsockets = true;
        extraConfig = ''
          rewrite "^/ollama/(.*)$" "/$1" break;
        '';
      };
    };

    services.ollama = {
      enable = true;
      acceleration = "rocm";
      loadModels = ["qwen2.5-coder:7b"];
      openFirewall = true;
      rocmOverrideGfx = "10.3.0";
    };
    services.open-webui = {
      enable = true;
      openFirewall = true;
      environment = {
        ANONYMIZED_TELEMETRY = "False";
        DO_NOT_TRACK = "True";
        SCARF_NO_ANALYTICS = "True";
        WEBUI_URL = "http://earthbound.fin-alioth.ts.net/open-webui";
      };
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
  };
}
