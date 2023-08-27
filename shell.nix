# Shell for bootstrapping flake-enabled nix and home-manager
# You can enter it through 'nix develop' or (legacy) 'nix-shell'
{
  pkgs ?
  # If pkgs is not defined, instanciate nixpkgs from locked commit
  let
    lock = builtins.fromJSON (builtins.readFile ./flake.lock);
    inherit (lock.nodes.nixpkgs.locked) owner repo rev narHash;
    nixpkgs = fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      sha256 = narHash;
    };
  in
    import nixpkgs {
      config = {
        allowUnfree = true;
        allowBroken = true;
      };
      overlays = [];
    },
  ...
}: {
  default = pkgs.mkShell {
    NIX_CONFIG = "extra-experimental-features = nix-command flakes repl-flake";
    nativeBuildInputs = with pkgs; [
      nix
      home-manager
      git
      cachix
      # nvfetcher
      sops
      ssh-to-age
      gnupg
      age
    ];
  };
}
