{
  config,
  lib,
  pkgs,
  ...
}: {
  programs = {
    bat = {
      enable = true;
      extraPackages = with pkgs.bat-extras; [
        batdiff
        batgrep
        batman
        batpipe
        batwatch
        prettybat
      ];
      config = {
        theme = "OneHalfDark";
      };
    };
  };
}
