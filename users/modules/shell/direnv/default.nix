{ config
, lib
, ...
}:
with lib; let
  cfg = config.custom.direnv;
in
{
  options.custom.direnv = {
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
