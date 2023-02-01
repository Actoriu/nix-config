{ lib }: {
  droid = import ./droid.nix;
  home = import ./home.nix;
  nixos = import ./nixos.nix;
}
