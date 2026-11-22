{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  makeWrapper,
  claude-code,
}:
buildNpmPackage (finalAttrs: {
  pname = "claude-agent-acp";
  version = "0.109.1";

  src = fetchFromGitHub {
    owner = "agentclientprotocol";
    repo = "claude-agent-acp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-1nX95rqpzbbPk5OFcpVJLfewemIX3GW7BhYXP9oZboE=";
  };

  npmDepsHash = "sha256-yz+zSb0XRVowNbRU9qvNsi0Wsc+cTZ2Iqlg+vqCuA2c=";

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
