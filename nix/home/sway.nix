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

  programs.swayr = {
    enable = true;
    systemd.enable = true;
    # settings.format.icons_dirs = [
    #   "/run/current-system/sw/share/icons/breeze-dark/apps/"
    # ];
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
          "Alt+space" = "exec --no-startup-id wofi --show drun,run";
          "Alt+Tab" = "exec swayr switch-window";

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
      floating.titlebar = false;
      focus.followMouse = false;
      startup = [
        {command = "firefox";}
        {
          command = "systemctl --user restart kanshi";
          always = true;
        }
        {
          command = "systemctl --user restart waybar";
          always = true;
        }
        {
          command = "systemctl --user restart swayidle";
          always = true;
        }
        {
          command = "systemctl --user restart swayr";
          always = true;
        }
      ];
      window.titlebar = false;
      workspaceAutoBackAndForth = true;
    };
    wrapperFeatures.gtk = true;
  };

  programs.waybar = {
    enable = true;
    systemd.enable = true;
    style = pkgs.lib.readFile ./waybar.css;
  };

  home.file.".hm-graphical-session".text = pkgs.lib.concatStringsSep "\n" [
    "export MOZ_ENABLE_WAYLAND=1"
    "export NIXOS_OZONE_WL=1" # Electron
  ];
  home.sessionVariables = {
    XDG_CURRENT_DESKTOP = "sway";
  };

  services.cliphist.enable = true;

  services.kanshi = {
    enable = true;
    settings = [
      {
        profile = {
          name = "home-office";
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
      }
    ];
  };

  services.swayidle = {
    enable = true;
    events = [
      {
        event = "before-sleep";
        command = "${pkgs.swaylock-fancy}/bin/swaylock-fancy -fF";
      }
      {
        event = "lock";
        command = "lock";
      }
    ];
    timeouts = [
      {
        timeout = 600;
        command = "${pkgs.sway}/bin/swaymsg \"output * power off\"";
        resumeCommand = "${pkgs.sway}/bin/swaymsg \"output * power on\"";
      }
    ];
  };

  home.packages = with pkgs; [
    grim
    mako # notifications
    slurp
    wl-clipboard
    libsForQt5.qt5ct
    libsForQt5.qtstyleplugin-kvantum
  ];

  xdg = {
    portal = {
      enable = pkgs.stdenv.isLinux;
      config = {
        sway = {
          default = ["gtk"];
          "org.freedesktop.impl.portal.Screenshot" = ["wlr"];
          "org.freedesktop.impl.portal.ScreenCast" = ["wlr"];
        };
      };
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
      xdgOpenUsePortal = true;
    };
    userDirs = {
      enable = pkgs.stdenv.isLinux;
      createDirectories = true;
    };
  };
}
