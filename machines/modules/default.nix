# { ... }: {
#   imports = [
#     ./fonts
#     ./hardware
#     ./locale
#     ./loader
#     ./network
#     ./power-management
#     ./usershell
#     ./version
#   ];
# }

{ pkgs
, lib
, ...
}:

let
  folder = ./.;
  toImport = name: value: folder + ("/" + name);
  filterCaches = key: value: value == "regular" && lib.hasSuffix ".nix" key && key != "default.nix";
  imports = lib.mapAttrsToList toImport (lib.filterAttrs filterCaches (builtins.readDir folder));
in
{
  inherit imports;
}
