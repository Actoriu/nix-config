{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.home-manager.locale;
in {
  options.custom.home-manager.locale = {
    enable = mkEnableOption "Enable support for locale.";

    inputMethod = mkOption {
      type = types.nullOr (types.enum ["fcitx5" "ibus"]);
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

  config = mkIf (cfg.enable && config.custom.non-nixos) (mkMerge [
    (mkIf (cfg.inputMethod != null) {
      i18n.inputMethod.enabled = cfg.inputMethod;
    })
    (mkIf (cfg.locale != null && cfg.locale == "zh_CN") {
      fonts.fontconfig.enable = true;
    })
    (mkIf (cfg.locale != null && cfg.locale == "zh_CN" && cfg.inputMethod == "fcitx5") {
      i18n.inputMethod.fcitx5 = {
        addons = with pkgs; [
          fcitx5-rime
        ];
      };
    })
    (mkIf (cfg.locale != null && cfg.locale == "zh_CN" && cfg.inputMethod == "ibus") {
      i18n.inputMethod.ibus.engines = with pkgs.ibus-engines; [rime];
    })
  ]);
}
