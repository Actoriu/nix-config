{
  config,
  lib,
  pkgs,
  ...
}: {
  users.defaultUserShell = pkgs.zsh;
  programs.zsh.enable = true;
  environment.pathsToLink = ["/share/zsh"];
}
