{inputs, pkgs, ...}: {
  flake = {
    overlays = import ../overlays {inherit inputs;};
    packages = import ../pkgs {inherit pkgs;}
  };
}
