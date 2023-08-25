{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.private;
in {
  options.private = {
    enable = mkEnableOption "Enable private customization support for nix.";
    platform = mkOption {
      type = types.nullOr (types.enum ["Darwin" "Home-manager" "Nixos" "Nix-on-droid"]);
      default = null;
      example = "Home-manager";
      description = "Enable support for the nix installation platform.";
    };
  };
}
