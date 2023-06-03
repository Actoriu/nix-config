{
  config,
  lib,
  pkgs,
  ...
}: {
  programs = {
    mpv = {
      enable = cfg.enable;
    };
  };
}
