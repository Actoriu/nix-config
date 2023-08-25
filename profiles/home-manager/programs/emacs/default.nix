{
  config,
  lib,
  pkgs,
  ...
}: {
  home = {
    packages = with pkgs; [
      guile_3_0
      ripgrep
      ugrep
      translate-shell
    ];
  };
}
