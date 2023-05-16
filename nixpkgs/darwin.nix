{ config, lib, pkgs, ... }:

{
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "uninstall";
    };
    brews = [
      "dotnet"
      "emacs-mac"
      "nuget"
      "pyenv"
    ];
    casks = [
      "aldente"
      "balenaetcher"
      "bitwarden"
      "firefox"
      "iina"
      "karabiner-elements"
      "logitech-unifying"
      "lunar"
      "mullvadvpn"
      "multipatch"
      "openemu"
      "scroll-reverser"
      "snes9x"
      "steam"
      "transmission"
      "visualboyadvance-m"
      "yt-music"
    ];
    taps = [ "homebrew/cask-drivers" "railwaycat/emacsmacport" ];
  };

  services = {
    skhd = {
      enable = true;
      package = pkgs.skhd;
      # Note that the `alt` keyword in skhd's config is the Option key.
      skhdConfig = lib.concatStringsSep "\n" [
        # Window control
        "alt - f : yabai -m window --toggle native-fullscreen"

        # Focus window
        "alt - k : yabai -m window --focus north"
        "alt - j : yabai -m window --focus south"
        "alt - h : yabai -m window --focus west"
        "alt - l : yabai -m window --focus east"

        # Swap windows
        "shift + alt - k : yabai -m window --swap north"
        "shift + alt - j : yabai -m window --swap south"
        "shift + alt - h : yabai -m window --swap west"
        "shift + alt - l : yabai -m window --swap east"

        # Focus space
        "alt - 1 : yabai -m space --focus 1"
        "alt - 2 : yabai -m space --focus 2"
        "alt - 3 : yabai -m space --focus 3"
        "alt - 4 : yabai -m space --focus 4"
        "alt - 5 : yabai -m space --focus 5"
        "alt - 6 : yabai -m space --focus 6"
        "alt - 7 : yabai -m space --focus 7"
        "alt - 8 : yabai -m space --focus 8"
        "alt - 9 : yabai -m space --focus 9"
        "alt - right : yabai -m space --focus next || yabai -m space --focus first"
        "alt - left : yabai -m space --focus prev || yabai -m space --focus last"

        # Send to space
        "shift + alt - 1 : yabai -m window --space 1; yabai -m space --focus 1"
        "shift + alt - 2 : yabai -m window --space 2; yabai -m space --focus 2"
        "shift + alt - 3 : yabai -m window --space 3; yabai -m space --focus 3"
        "shift + alt - 4 : yabai -m window --space 4; yabai -m space --focus 4"
        "shift + alt - 5 : yabai -m window --space 5; yabai -m space --focus 5"
        "shift + alt - 6 : yabai -m window --space 6; yabai -m space --focus 6"
        "shift + alt - 7 : yabai -m window --space 7; yabai -m space --focus 7"
        "shift + alt - 8 : yabai -m window --space 8; yabai -m space --focus 8"
        "shift + alt - 9 : yabai -m window --space 9; yabai -m space --focus 9"
      ];
    };
    spacebar = {
      enable = true;
      package = pkgs.spacebar;
      config = {
        position = "top";
        display = "all";
        height = 25;
        title = "on";
        spaces = "on";
        clock = "on";
        power = "on";
        padding_left = 20;
        padding_right = 20;
        spacing_left = 25;
        spacing_right = 15;
        text_font = ''"Menlo:Regular:12.0"'';
        icon_font = ''"Font Awesome 5 Free:Solid:12.0"'';
        background_color = "0xff202020";
        foreground_color = "0xffa8a8a8";
        power_icon_color = "0xffcd950c";
        battery_icon_color = "0xffd75f5f";
        dnd_icon_color = "0xffa8a8a8";
        clock_icon_color = "0xffa8a8a8";
        power_icon_strip = " ";
        space_icon = "•";
        space_icon_strip = "1 2 3 4 5 6 7 8 9 10";
        spaces_for_all_displays = "on";
        display_separator = "on";
        display_separator_icon = "";
        space_icon_color = "0xff458588";
        space_icon_color_secondary = "0xff78c4d4";
        space_icon_color_tertiary = "0xfffff9b0";
        clock_icon = "";
        dnd_icon = "";
        clock_format = ''"%y-%m-%d %R"'';
        # right_shell = "on";
        # right_shell_icon = "";
        # right_shell_command = "whoami";
      };
    };
    yabai = {
      enable = true;
      package = pkgs.yabai;
      config = {
        layout = "bsp";
        auto_balance = "on";
        window_border = "on";
        window_border_placement = "inset";
        window_border_width = 1;
        external_bar = "all:25:0";
      };
      extraConfig = lib.concatStringsSep "\n" [
        "yabai -m rule --add app='Preferences' manage=off layer=above"
        "yabai -m rule --add app='^(Opening)' manage=off layer=above"
        "yabai -m rule --add app='^System Information$' manage=off layer=above"
        "yabai -m rule --add app='^System Preferences$' manage=off layer=above"
        "yabai -m rule --add app='^System Settings' manage=off layer=above"
        "yabai -m rule --add app='^MultiPatch' manage=off layer=above"
      ];
    };
  };

  system.keyboard.enableKeyMapping = true; # needed for skhd
}
