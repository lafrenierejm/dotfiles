{
  pkgs,
  lib,
  ...
}: let
  mod = "Mod4";
in {
  programs.wofi = {
    enable = true;
    settings = {
      allow_markup = true;
      width = 250;
    };
  };
  wayland.windowManager.sway = {
    enable = true;
    config = {
      modifier = mod;
      keybindings = lib.attrsets.mergeAttrsList [
        (lib.attrsets.mergeAttrsList (map (num: let
          ws = toString num;
        in {
          "${mod}+${ws}" = "workspace ${ws}";
          "${mod}+Ctrl+${ws}" = "move container to workspace ${ws}";
        }) [1 2 3 4 5 6 7 8 9]))

        (lib.attrsets.concatMapAttrs (key: direction: {
            "${mod}+${key}" = "focus ${direction}";
            "${mod}+Ctrl+${key}" = "move ${direction}";
          }) {
            h = "left";
            j = "down";
            k = "up";
            l = "right";
          })

        {
          "${mod}+Return" = "exec --no-startup-id ${pkgs.kitty}/bin/kitty";
          "${mod}+space" = "exec --no-startup-id wofi --show drun,run";

          "${mod}+x" = "kill";

          "${mod}+a" = "focus parent";
          "${mod}+e" = "layout toggle split";
          "${mod}+f" = "fullscreen toggle";
          "${mod}+g" = "split h";
          "${mod}+s" = "layout stacking";
          "${mod}+v" = "split v";
          "${mod}+w" = "layout tabbed";

          "${mod}+Shift+r" = "exec swaymsg reload";
          "--release Print" = "exec --no-startup-id ${pkgs.sway-contrib.grimshot}/bin/grimshot copy area";
          "${mod}+Shift+l" = "exec ${pkgs.swaylock-fancy}/bin/swaylock-fancy";
          "${mod}+Ctrl+q" = "exit";
        }
      ];
      bars = [];
      focus.followMouse = false;
      startup = [
        {command = "firefox";}
        {
          command = "systemctl --user restart kanshi";
          always = true;
        }
      ];
      workspaceAutoBackAndForth = true;
    };
    systemd.enable = true;
    wrapperFeatures = {gtk = true;};
  };

  programs.waybar = {
    enable = true;
    systemd.enable = true;
  };

  home.file.".hm-graphical-session".text = pkgs.lib.concatStringsSep "\n" [
    "export MOZ_ENABLE_WAYLAND=1"
    "export NIXOS_OZONE_WL=1" # Electron
  ];

  services.cliphist.enable = true;

  services.kanshi = {
    enable = true;

    profiles = {
      home_office = {
        outputs = [
          {
            criteria = "DP-2";
            scale = 2.0;
            status = "enable";
            position = "0,0";
          }
          {
            criteria = "DP-1";
            scale = 2.0;
            status = "enable";
            position = "1920,0";
          }
          {
            criteria = "DP-3";
            scale = 2.0;
            status = "enable";
            position = "3840,0";
          }
        ];
      };
    };
  };

  services.gnome-keyring.enable = true;

  home.packages = with pkgs; [
    grim
    slurp
    wl-clipboard
    mako # notifications
  ];
}
