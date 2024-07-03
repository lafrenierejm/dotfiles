{
  lib,
  realName,
}: {
  maildirBasePath = "Mail";
  accounts = let
    signature = {
      showSignature = true;
      text = lib.concatStringSep [
        "-- "
        realName
      ];
    };
  in {
    "xyz.lafreniere" = {
      inherit signature realName;
      address = "git@lafreniere.xyz";
      userName = "notmuch";
      flavor = "fastmail.com";
      mujmap = {
        enable = true;
        notmuchSetupWarning = true;
        settings = {
          auto_create_new_mailboxes = true;
        };
      };
      notmuch.enable = true;
      passwordCommand = "bw get password notmuch@fastmail.com";
      primary = true;
    };
  };
}
