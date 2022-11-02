{ inputs
, lib
, ...
}:

{
  my = {
    mkNixOS = import ./mkNixOS.nix { inherit inputs lib; };
    mkHome = import ./mkHome.nix { inherit inputs lib; };
    mkDroid = import ./mkDroid { inherit inputs lib; };
  };
}
