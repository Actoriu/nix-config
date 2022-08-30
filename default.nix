{ pkgs
, ...
}: {
  modules = import ./modules/nixos;
  homeManagerModules = import ./modules/home-manager;
}
// (import ./pkgs { inherit pkgs; })
  // (
  if builtins ? getFlake then
    builtins.getFlake (toString ./.)
  else
    (import ./lib/compat).defaultNix
)
