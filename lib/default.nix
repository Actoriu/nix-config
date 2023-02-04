{
  lib,
  inputs,
  outputs,
}:
let
version = lib.fileContents ./.version;
  in
{
  droid = import ./droid.nix {inherit lib inputs outputs version;};
  home = import ./home.nix {inherit lib inputs outputs version;};
  nixos = import ./nixos.nix {inherit lib inputs outputs version;};
}
