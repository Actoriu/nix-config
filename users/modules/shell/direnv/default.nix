{
  config,
  lib,
  ...
}:

with lib;

let
  cfg = config.custom.shell.direnv;
in
{
  options.custom.shell.direnv = {
    enable = mkEnableOption "Enable support for direnv.";
    nix-direnv = mkEnableOption "Enable support for nix-direnv.";
  };

  config = mkIf cfg.enable (mkMerge [
    { programs.direnv.enable = cfg.enable; }
    (mkIf cfg.nix-direnv {
      programs.direnv.nix-direnv = {
        enable = cfg.nix-direnv;
      };
    })
  ]);
}
