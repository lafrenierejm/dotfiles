{
  stdenv,
  zig,
  # Absolute path of the Emacs executable to spawn.
  emacsPath,
  # Bundle identifier; the code-signing requirement should be pinned to it.
  bundleId,
}:
stdenv.mkDerivation {
  pname = "emacs-launcher";
  version = "2";

  src = ./.;

  nativeBuildInputs = [zig.hook];

  zigBuildFlags = ["-Demacs-path=${emacsPath}"];

  # An unsigned bundle; `nix/darwin.nix` copies and signs it at a fixed path.
  # The hook installs only `bin/`, so wrap the result in the bundle layout.
  postInstall = ''
    app="$out/Applications/Emacs Launcher.app/Contents"
    mkdir -p "$app/MacOS"
    mv $out/bin/emacs-launcher "$app/MacOS/emacs-launcher"
    rmdir $out/bin
    substitute Info.plist "$app/Info.plist" --subst-var-by bundleId ${bundleId}
  '';

  meta.platforms = ["aarch64-darwin" "x86_64-darwin"];
}
