{pkgs, ...}: let
  mod = "Mod4";
  workspaces = {
    ws1 = "1";
    ws2 = "2";
    ws3 = "3";
    ws4 = "4";
    ws5 = "5";
    ws6 = "6";
    ws7 = "7";
    ws8 = "8";
    ws9 = "9";
    ws0 = "0";
  };
in {
  wayland.windowManager.sway = {
    enable = true;
    config = {
      modifier = mod;
      keybindings = {
        "${mod}+Return" = "exec --no-startup-id ${pkgs.kitty}/bin/kitty";

        # Navigation
        "${mod}+0" = "workspace ${workspaces.ws0}";
        "${mod}+1" = "workspace ${workspaces.ws1}";
        "${mod}+2" = "workspace ${workspaces.ws2}";
        "${mod}+3" = "workspace ${workspaces.ws3}";
        "${mod}+4" = "workspace ${workspaces.ws4}";
        "${mod}+5" = "workspace ${workspaces.ws5}";
        "${mod}+6" = "workspace ${workspaces.ws6}";
        "${mod}+7" = "workspace ${workspaces.ws7}";
        "${mod}+8" = "workspace ${workspaces.ws8}";
        "${mod}+9" = "workspace ${workspaces.ws9}";
        "${mod}+h" = "focus left";
        "${mod}+j" = "focus down";
        "${mod}+k" = "focus up";
        "${mod}+l" = "focus right";
        "${mod}+x" = "kill";

        # Modes
        "${mod}+a" = "focus parent";
        "${mod}+e" = "layout toggle split";
        "${mod}+f" = "fullscreen toggle";
        "${mod}+g" = "split h";
        "${mod}+s" = "layout stacking";
        "${mod}+space" = "focus mode_toggle";
        "${mod}+v" = "split v";
        "${mod}+w" = "layout tabbed";

        # Meta
        "${mod}+Shift+r" = "exec swaymsg reload";
        "--release Print" = "exec --no-startup-id ${pkgs.grimshot}/bin/grimshot copy area";
        "${mod}+Ctrl+l" = "exec ${pkgs.swaylock-fancy}/bin/swaylock-fancy";
        "${mod}+Ctrl+q" = "exit";
      };
      startup = [
        {command = "firefox";}
      ];
    };
  };

  home.file.".hm-graphical-session".text = pkgs.lib.concatStringsSep "\n" [
    "export MOZ_ENABLE_WAYLAND=1"
    "export NIXOS_OZONE_WL=1" # Electron
  ];

  services.kanshi = {
    enable = true;

    profiles = {
      home_office = {
        outputs = [
          {
            criteria = "DP-1";
            scale = 2.0;
            status = "enable";
          }
          {
            criteria = "DP-2";
            scale = 2.0;
            status = "enable";
          }
          {
            criteria = "DP-3";
            scale = 2.0;
            status = "enable";
          }
        ];
      };
    };
  };

  home.packages = with pkgs; [
    grim
    slurp
    wl-clipboard
    mako # notifications
  ];
}
