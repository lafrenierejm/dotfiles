{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  gtk3,
  hicolor-icon-theme,
  jdupes,
}:
stdenvNoCC.mkDerivation {
  pname = "whitesur-combined";
  version = "0.0.0";
  srcs = [
    (fetchFromGitHub {
      name = "cursors";
      owner = "vinceliuice";
      repo = "WhiteSur-cursors";
      rev = "e190baf618ed95ee217d2fd45589bd309b37672b";
      hash = "sha256-hFtfq8F6KeqUEBlypPCr/EKq6rif/g868vJd8c06c1I=";
    })
    (fetchFromGitHub {
      name = "icons";
      owner = "vinceliuice";
      repo = "WhiteSur-icon-theme";
      tag = "2025-08-02";
      hash = "sha256-oBKDvCVHEjN6JT0r0G+VndzijEWU9L8AvDhHQTmw2E4=";
    })
  ];
  sourceRoot = ".";
  nativeBuildInputs = [
    gtk3
    jdupes
  ];
  buildInputs = [
    hicolor-icon-theme
  ];

  dontPatchELF = true;
  dontRewriteSymlinks = true;
  dontDropIconThemeCache = true;

  postPatch = ''
    patchShebangs ./icons/install.sh
  '';

  installPhase = ''
    runHook preInstall

    install -dm 755 "$out/share/icons/WhiteSur"

    ./icons/install.sh --dest "$out/share/icons" \
      --name WhiteSur \
      --theme default

    cp -r cursors/dist/cursors "$out/share/icons/WhiteSur/cursors"
    cp -r cursors/dist/cursors_scalable "$out/share/icons/WhiteSur/cursors_scalable"
    jdupes --link-soft --recurse "$out/share"

    runHook postInstall
  '';

  meta = {
    description = "Combined WhiteSur package";
    homepage = "https://github.com/vinceliuice/WhiteSur-cursors";
    license = lib.licenses.gpl3Only;
    maintainers = with lib.maintainers; [lafrenierejm];
    platforms = lib.platforms.linux;
  };
}
