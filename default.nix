{ pkgs
, ...
}: {
  modules = import ./modules/nixos;
  homeManagerModules = import ./modules/home-manager;
}
// (import ./pkgs { inherit pkgs; })
