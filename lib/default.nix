{
  lib,
  inputs,
} @ args: {
  droid = import ./droid.nix {inherit args;};
  home = import ./home.nix {inherit args;};
  nixos = import ./nixos.nix {inherit args;};
}
