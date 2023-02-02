{ lib
, inputs
, ...
}:

{
  droid = import ./droid.nix { inherit lib inputs; };
  home = import ./home.nix { inherit lib inputs; };
  nixos = import ./nixos.nix { inherit lib inputs; };
}
