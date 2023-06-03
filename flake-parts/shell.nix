{...}: {
  imports = [
    inputs.devshell.flakeModule
    inputs.mission-control.flakeModule
  ];

  perSystem =
    { config,
      lib,
      pkgs,
      ...
    }: {
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.flake-root.devShell
            config.mission-control.devShell
            config.pre-commit-hooks.devShell
          ];
        };
    };
}
