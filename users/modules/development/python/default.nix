{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.custom.development.python;
in
{
  options.custom.development.python = {
    enable = mkEnableOption "Enable support for python language.";
  };

  config = mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        (python3.withPackages
          (ps: with ps; [
            flake8
            importmagic
            ipython
            isort
            python_magic
            setuptools
          ]))
      ];
    };
  };
}
