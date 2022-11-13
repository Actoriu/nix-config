{ inputs
, lib
, pkgs
, ...
}:

{
  imports =
    let
      dir = ./.;
    in
    builtins.map (file: ./. + ("/" + file))
      (builtins.filter (file: type: (type == "regular" && lib.hasSuffix ".nix" file) && (type != "directory")) (builtins.attrNames (builtins.readDir ./.)));
}

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
