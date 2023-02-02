{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.python;
in {
  options.custom.python = {
    enable = mkEnableOption "Enable support for python language.";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      (python3.withPackages
        (ps:
          with ps; [
            flake8
            importmagic
            ipython
            isort
            python_magic
            setuptools
          ]))
    ];
  };
}
