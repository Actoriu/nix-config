{...}: {
  imports = [
    ./droid.nix
    # ./formatter.nix
    ./home-manager.nix
    ./nixos.nix
    # ./overlays.nix
    ./pre-commit.nix
    ./shell/devshell.nix
    ./treefmt.nix
  ];
}
