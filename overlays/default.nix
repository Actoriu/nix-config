{inputs, ...}: let
  addPatches = pkg: patches:
    pkg.overrideAttrs (oldAttrs: {
      patches = (oldAttrs.patches or []) ++ patches;
    });
in {
  # For every flake input, aliases 'pkgs.inputs.${flake}' to
  # 'inputs.${flake}.packages.${pkgs.system}' or
  # 'inputs.${flake}.legacyPackages.${pkgs.system}' or
  flake-inputs = final: _: {
    inputs =
      builtins.mapAttrs
      (_: flake: (flake.packages or flake.legacyPackages or {}).${final.system} or {})
      inputs;
  };

  # Adds my custom packages
  additions = final: prev: import ../pkgs {pkgs = final;};

  # Modifies existing packages
  modifications = final: prev: {
  };

  # When applied, the spacemacs set (declared in the flake inputs) will
  # be accessible through 'pkgs.spacemacs'
  alacritty-theme = final: prev: {
    alacritty-theme = inputs.alacritty-theme;
  };

  spacemacs = final: prev: {
    spacemacs = inputs.spacemacs;
  };
}
