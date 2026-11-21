{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  makeWrapper,
  claude-code,
}:
buildNpmPackage (finalAttrs: {
  pname = "claude-agent-acp";
  version = "0.109.0";

  src = fetchFromGitHub {
    owner = "agentclientprotocol";
    repo = "claude-agent-acp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-I2m+q06rB5hxi2J1Ai5mdcbZYD3p0LQ4/KAruk0jjQs=";
  };

  npmDepsHash = "sha256-qSKBpAYu2/2B5uBnkS+yqdqyUDfHsbIyTgk824GPcH4=";

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
