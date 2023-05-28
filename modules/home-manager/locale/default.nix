{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.home.locale;
in {
  options.custom.home.locale = {
    enable = mkEnableOption "Enable support for locale.";

    inputMethod = mkOption {
      type = types.nullOr (types.enum ["fcitx5" "uim"]);
      default = null;
      example = "fcitx5";
      description = "Enable support for inputMethod.";
    };

    locale = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "zh_CN";
      description = "Enable support for locale.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.inputMethod != null && config.custom.non-nixos) {
      i18n.inputMethod.enabled = cfg.inputMethod;
    })
    (mkIf (cfg.locale != null && config.custom.non-nixos) {
      fonts.fontconfig.enable = true;
    })
    (mkIf (cfg.locale != null && cfg.locale == "zh_CN" && cfg.inputMethod == "fcitx5" && config.custom.non-nixos) {
      i18n.inputMethod.fcitx5 = {
        addons = with pkgs; [
          fcitx5-rime
        ];
      };
    })
  ]);
}
