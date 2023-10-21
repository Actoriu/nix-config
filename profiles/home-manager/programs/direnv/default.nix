{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.direnv = {
    enable = true;
    config = {
      strict_env = true;
      warn_timeout = "30s";
    };
    nix-direnv = {
      enable = true;
    };
  };
}
