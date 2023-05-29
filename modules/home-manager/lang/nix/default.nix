{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.custom.nix;
in {
  options.custom.nix = {
    enable = lib.mkEnableOption "Enable support for nix language.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      rnix-lsp
    ];
  };
}
