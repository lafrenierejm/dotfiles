{
  config,
  lib,
  pkgs,
  hostname,
  ...
}: let
  cfg = config.services.restic-backup;
in {
  options.services.restic-backup.enable = lib.mkEnableOption "restic backup of ~/Documents to B2";

  config = lib.mkIf cfg.enable {
    age.secrets."restic-password" = {
      file = ../../restic/password.age;
      owner = config.system.primaryUser;
    };
    age.secrets."darwin/${hostname}/b2-key-id" = {
      file = ./. + "/${hostname}/b2-account-id.age";
      owner = config.system.primaryUser;
    };
    age.secrets."darwin/${hostname}/b2-key" = {
      file = ./. + "/${hostname}/b2-account-key.age";
      owner = config.system.primaryUser;
    };

    launchd.agents."restic-backup" = {
      path = with pkgs; [restic];
      script = ''
        set -euo pipefail
        export RESTIC_REPOSITORY="s3:s3.us-west-000.backblazeb2.com/${hostname}"
        export AWS_ACCESS_KEY_ID="$(< ${config.age.secrets."darwin/${hostname}/b2-account-id".path})"
        export AWS_SECRET_ACCESS_KEY="$(< ${config.age.secrets."darwin/${hostname}/b2-account-key".path})"
        restic backup "$HOME/Documents" --exclude-file=${../../restic/exclude-file.txt}
      '';
      serviceConfig = {
        StartCalendarInterval = [
          {
            Hour = 8;
            Minute = 0;
            Weekday = 0;
          } # Sunday at 08:00
        ];
        RunAtLoad = false;
        StandardOutPath = "/Users/${config.system.primaryUser}/Library/Logs/restic-backup.log";
        StandardErrorPath = "/Users/${config.system.primaryUser}/Library/Logs/restic-backup.log";
      };
    };
  };
}
