{
  lib,
  inputs,
  outputs,
}: {
  droid = import ./droid.nix {inherit lib inputs outputs;};
  home = import ./home.nix {inherit lib inputs outputs;};
  nixos = import ./nixos.nix {inherit lib inputs outputs;};
}
