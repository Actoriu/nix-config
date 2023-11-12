{inputs, ...}: {
  imports = [
    inputs.flake-root.flakeModule
    inputs.treefmt-nix.flakeModule
  ];

  perSystem = {
    config,
    pkgs,
    ...
  }: {
    treefmt = {
      # projectRootFile = "flake.nix";
      inherit (config.flake-root) projectRootFile;
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

    # perSystem.treefmt.flakeFormatter
    # Enables treefmt the default formatter used by the nix fmt command
    # Default: true
    # formatter = config.treefmt.build.wrapper;
  };
}
