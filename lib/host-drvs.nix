{ self
, ...
}:

system:

let
  inherit (self.pkgs.${system}) lib linkFarm;

  hosts = import ./hosts.nix;

  nixosDrvs = lib.mapAttrs (_: nixos: nixos.config.system.build.toplevel) self.nixosConfigurations;
  homeDrvs = lib.mapAttrs (_: home: home.activationPackage) self.homeConfigurations;
  darwinDrvs = lib.mapAttrs (_: darwin: darwin.system) self.darwinConfigurations;
  droidDrvs = lib.mapAttrs (_: droid: droid.activationPackage) self.nixOnDroidConfigurations;
  hostDrvs = nixosDrvs // homeDrvs // darwinDrvs // droidDrvs;

  structuredHostDrvs = lib.mapAttrsRecursiveCond
    (as: !(as ? "type" && (lib.elem as.type [ "darwin" "droid" "home-manager" "nixos" ])))
    (path: _: hostDrvs.${lib.last path})
    hosts;

  structuredHostFarms = lib.mapAttrsRecursiveCond
    (as: !(lib.any lib.isDerivation (lib.attrValues as)))
    (path: values:
      (linkFarm
        (lib.concatStringsSep "-" path)
        (lib.mapAttrsToList (name: path: { inherit name path; }) values)) //
      values
    )
    structuredHostDrvs;
in
structuredHostFarms
