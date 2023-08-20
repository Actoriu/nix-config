{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.i18n;
in {
  options.custom.i18n = {
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

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.inputMethod != null) {
      i18n.inputMethod.enabled = cfg.inputMethod;
    })
    (mkIf (cfg.locale != null && cfg.locale == "zh_CN") {
      fonts = {
        packages = with pkgs; [
          hanazono
          noto-fonts-cjk-sans
          noto-fonts-cjk-serif
          sarasa-gothic
        ];
        fontconfig = {
          defaultFonts = {
            monospace = ["Noto Sans Mono CJK SC"];
            sansSerif = ["Noto Sans CJK SC"];
            serif = ["Noto Serif CJK SC" "HanaMinA" "HanaMinB"];
          };
        };
      };
      i18n = {
        defaultLocale = "zh_CN.UTF-8";
        supportedLocales = ["en_US.UTF-8/UTF-8" "zh_CN.UTF-8/UTF-8"];
      };
      time.timeZone = "Asia/Shanghai";
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
