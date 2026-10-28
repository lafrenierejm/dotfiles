{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  makeWrapper,
  claude-code,
}:
buildNpmPackage (finalAttrs: {
  pname = "claude-agent-acp";
  version = "0.99.0";

  src = fetchFromGitHub {
    owner = "agentclientprotocol";
    repo = "claude-agent-acp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-O+5crPHlzBIFYobKh2J4j/I/Ej+CThSSGNJidh5TVL8=";
  };

  npmDepsHash = "sha256-KP/Rupaukh3HL23pTDprmh7qKYNhap8QFFjGqm9Ir0o=";

  nativeBuildInputs = [makeWrapper];

  postInstall = ''
    wrapProgram $out/bin/claude-agent-acp \
      --set-default CLAUDE_CODE_EXECUTABLE ${lib.getExe claude-code}
  '';

  meta = {
    description = "ACP-compatible coding agent powered by the Claude Agent SDK";
    homepage = "https://github.com/agentclientprotocol/claude-agent-acp";
    changelog = "https://github.com/agentclientprotocol/claude-agent-acp/releases/tag/v${finalAttrs.version}";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [
      amadejkastelic
      storopoli
    ];
    mainProgram = "claude-agent-acp";
  };
})
