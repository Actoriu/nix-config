{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.private.graphical;
in {
  options.private.graphical = {
    enable = mkEnableOption "Enable support for graphical.";

    display = mkOption {
      type = types.nullOr (types.enum ["x11" "wayland"]);
      default = null;
      example = "x11";
      description = "What display protocol to use.";
    };

    platform = mkOption {
      type = types.nullOr (types.enum ["Darwin" "Home-manager" "Nixos" "Nix-on-droid"]);
      default = null;
      example = "Home-manager";
      description = "Enable support for the nix installation platform.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      /*
        assertions = [
        {
          assertion = (countAttrs (n: v: n == "enable" && value) cfg) < 2;
          message = "Prevent DE/WM > 1 from being enabled.";
        }
      ];
      */

    }
  ]);
}
