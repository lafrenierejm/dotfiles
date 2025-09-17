{
  pkgs,
  lib,
  ...
}: let
  mod = "Mod4";
  workspaceToDisplay = {
    "1" = "DP-3";
    "2" = "DP-3";
    "3" = "DP-3";
    "4" = "DP-2";
    "5" = "DP-2";
    "6" = "DP-2";
    "7" = "DP-1";
    "8" = "DP-1";
    "9" = "DP-1";
  };
  workspaceToDisplays = lib.attrsets.mapAttrs (workspace: display: [display]) workspaceToDisplay;
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

  programs.swayimg.enable = true;

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
          "${mod}+Return" = "exec --no-startup-id ${pkgs.ghostty}/bin/ghostty";
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
      focus.mouseWarping = false;
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
    settings = {
      mainbar = {
        modules-left = [
          "sway/workspaces"
        ];
        modules-center = [
          "sway/window"
        ];
        modules-right = [
          # "mpd"
          "idle_inhibitor"
          # "pipewire"
          "network"
          "cpu"
          "memory"
          "temperature"
          "clock"
          "tray"
        ];
        clock = {
          format = "{:%H:%M %Y-%m-%d}";
          tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
        };
        cpu = {
          format = "{usage}% ";
          tooltip = true;
        };
        memory = {
          format = "{}% ";
        };
        network = {
          format-wifi = "{essid} ({signalStrength}%) ";
          format-ethernet = "{ipaddr}/{cidr} ";
          tooltip-format = "{ifname} via {gwaddr} ";
          format-linked = "{ifname} (No IP) ";
          format-disconnected = "Disconnected ⚠";
          format-alt = "{ifname}: {ipaddr}/{cidr}";
        };
        pipewire = {
          format = "{volume}% {icon} {format_source}";
          format-bluetooth = "{volume}% {icon} {format_source}";
          format-bluetooth-muted = " {icon} {format_source}";
          format-muted = " {format_source}";
          format-source = "{volume}% ";
          format-source-muted = "";
          format-icons = {
            headphone = "";
            hands-free = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = ["" "" ""];
          };
          on-click = "pavucontrol";
        };
        temperature = {
          critical-threshold = 80;
          format = "{temperatureC}°C {icon}";
          format-icons = ["" "" ""];
        };
        workspaces = {
          sort-by-number = true;
          persistent-workspaces = workspaceToDisplays;
        };
      };
      dock = {
        layer = "top";
        position = "bottom";
        mode = "invisible";
        height = 41;
        width = 2;
        modules-center = [
          "sway/mode"
          "wlr/taskbar"
        ];
        margin = "4";
        spacing = "5";
        "sway/window" = {
          "max-length" = 50;
        };
        "wlr/taskbar" = {
          format = "{icon}";
          icon-size = 36;
          spacing = 3;
          on-click-middle = "close";
          tooltip-format = "{title}";
          ignore-list = [];
          on-click = "activate";
        };
      };
    };
    systemd.enable = true;
    # style = pkgs.lib.readFile ./waybar.css;
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
              criteria = "DP-1";
              scale = 2.0;
              status = "enable";
              position = "3840,0";
            }
            {
              criteria = "DP-2";
              scale = 2.0;
              status = "enable";
              position = "1920,0";
            }
            {
              criteria = "DP-3";
              scale = 2.0;
              status = "enable";
              position = "0,0";
            }
          ];
          exec =
            lib.mapAttrsToList
            (workspace: display: "${pkgs.sway}/bin/swaymsg workspace ${workspace}, move workspace to ${display}")
            workspaceToDisplay;
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
    swaylock-fancy
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
  };
}
