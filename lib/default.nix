{ inputs
, lib
, pkgs
, ...
}:

# let
#   inherit (lib) makeExtensible attrValues foldr;
#   inherit (modules) mapModules;

#   modules = import ./modules.nix {
#     inherit lib;
#     self.attrs = import ./attrs.nix { inherit lib; self = { }; };
#   };

#   mylib = makeExtensible (self:
#     with self; mapModules ./.
#       (file: import file { inherit self lib pkgs inputs; }));
# in
# mylib.extend
#   (self: super:
#     foldr (a: b: a // b) { } (attrValues super))
let
  mylib = lib.makeExtensible (self:
    let
      callLibs = file: import file { lib = self; };
    in
    {
      droid = callLibs ./droid.nix;
      home-manager = callLibs ./home-manager.nix;
      nixos = callLibs ./nixos;
    });
in
mylib
