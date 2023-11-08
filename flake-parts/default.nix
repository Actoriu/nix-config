{
  self,
  ...
}@inputs:
inputs.flake-parts.lib.mkFlake {inherit inputs;} {
  systems = ["aarch64-linux" "x86_64-linux"];
  imports = [
    # ./droid.nix
    ./formatter.nix
    # ./home-manager.nix
    # ./nixos.nix
    # ./overlays.nix
    ./pre-commit.nix
    ./shell
    ./trefmt.nix
  ];
};
