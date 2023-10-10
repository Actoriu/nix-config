{
  config,
  lib,
  pkgs,
  ...
}: {
  # i18n.glibcLocales = pkgs.glibcLocales.override {
  #   allLocales = false;
  #   locales = ["en_US.UTF-8/UTF-8" "zh_CN.UTF-8/UTF-8"];
  # };

  home = {
    language = {
      base = "zh_CN.UTF-8";
    };
    sessionVariables = {
      LANGUAGE = "zh_CN:en_US";
      LC_CTYPE = "zh_CN.UTF-8";
    };
  };
}
