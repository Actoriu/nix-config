{inputs, ...}: {
  imports = [
    inputs.flake-root.flakeModule
    inputs.treefmt-nix.flakeModule
  ];

  perSystem = {
    config,
    lib,
    pkgs,
    ...
  }: {
    treefmt.config = {
      inherit (config.flake-root) projectRootFile;
      package = pkgs.treefmt;
      programs = {
        alejandra.enable = true;
        prettier.enable = true;
        shellcheck.enable = true;
        shfmt.enable = true;
      };
      settings.formatter = {
        alejandra = {
          excludes = [
            "pkgs/_sources/generated.nix"
          ];
        };
        prettier = {
          excludes = [
            "pkgs/_sources/generated.json"
            ".github/renovate.json"
          ];
        };
      };
    };
  };
}
