{self, ...}: system: let
  inherit (self.pkgs.${system}) lib linkFarm;

  darwinDrvs = lib.mapAttrs (_: darwin: darwin.system) self.darwinConfigurations;
  droidDrvs = lib.mapAttrs (_: droid: droid.activationPackage) self.nixOnDroidConfigurations;
  homeDrvs = lib.mapAttrs (_: home: home.activationPackage) self.homeConfigurations;
  nixosDrvs = lib.mapAttrs (_: nixos: nixos.config.system.build.toplevel) self.nixosConfigurations;
  hostDrvs = darwinDrvs // droidDrvs // homeDrvs // nixosDrvs;

  structuredHostDrvs =
    lib.mapAttrsRecursiveCond
    (hostAttr: !(hostAttr ? "type" && (lib.elem hostAttr.type ["darwin" "droid" "homeManager" "nixos"])))
    (path: _: hostDrvs.${lib.last path})
    self.hosts;

  structuredHostFarms =
    lib.mapAttrsRecursiveCond
    (as: !(lib.any lib.isDerivation (lib.attrValues as)))
    (
      path: values:
        (linkFarm
          (lib.concatStringsSep "-" path)
          (lib.mapAttrsToList (name: path: {inherit name path;}) values))
        // values
    )
    structuredHostDrvs;
in
  structuredHostFarms
