# Shell for bootstrapping flake-enabled nix and home-manager
{ pkgs ? let
    # If pkgs is not defined, instanciate nixpkgs from locked commit
    lock = builtins.fromJSON (builtins.readFile ./flake.lock);
    inherit (lock.nodes.nixpkgs.locked) owner repo rev narHash;
    nixpkgs = fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      sha256 = narHash;
    };
    system = builtins.currentSystem;
    overlays = [ ]; # Explicit blank overlay to avoid interference
  in
  import nixpkgs { inherit system overlays; }
, ...
}: pkgs.mkShell
  {
    # Enable experimental features without having to specify the argument
    NIX_CONFIG = "experimental-features = nix-command flakes";
    nativeBuildInputs = with pkgs; [ nix home-manager git ];
  }
