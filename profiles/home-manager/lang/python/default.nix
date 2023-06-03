{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    (python3.withPackages
      (ps:
        with ps; [
          flake8
          importmagic
          ipython
          isort
          python-magic
          setuptools
        ]))
  ];
}
