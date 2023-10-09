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
      (
        _: flake: let
          legacyPackages = (flake.legacyPackages or {}).${final.system} or {};
          packages = (flake.packages or {}).${final.system} or {};
        in
          if legacyPackages != {}
          then legacyPackages
          else packages
      )
      inputs;
  };

  # Adds my custom packages
  additions = final: prev: import ../pkgs {pkgs = final;};

  # Modifies existing packages
  modifications = final: prev: {
  };

  # non flake inputs
  my-inputs = final: prev: {
    # alacritty-theme = inputs.alacritty-theme;
    # chemacs2 = inputs.chemacs2;
    # doom-emacs = inputs.doom-emacs;
    # spacemacs = inputs.spacemacs;
  };
}
