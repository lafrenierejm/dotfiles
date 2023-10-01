{
  config,
  pgks,
  ...
}: {
  services.mpd = {
    enable = true;
    musicDirectory = "${config.home.homeDirectory}/audio";
    playlistDirectory = "${config.xdg.dataHome}/mpd/playlist";
    dataDir = "${config.xdg.dataHome}/mpd";
  };
}
