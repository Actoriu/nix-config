{ inputs, ... }:
let
  additions = final: _prev: import ../pkgs { pkgs = final; };

  modifications = final: prev: {
    linuxPackages = prev.linuxPackages.extend (final: prev: rec {
      nvidiaPackages =
        prev.nvidiaPackages
        // {
          legacy_340 = prev.nvidiaPackages.legacy_340.overrideAttrs (oldAttrs: rec {
            patches =
              (oldAttrs.patches or [ ])
              ++ [
                ./nvidia-legacy-340/0001-kernel-5.7.patch
                ./nvidia-legacy-340/0002-kernel-5.8.patch
                ./nvidia-legacy-340/0003-kernel-5.9.patch
                ./nvidia-legacy-340/0004-kernel-5.10.patch
                ./nvidia-legacy-340/0005-kernel-5.11.patch
                ./nvidia-legacy-340/0006-kernel-5.14.patch
                ./nvidia-legacy-340/0007-kernel-5.15.patch
                ./nvidia-legacy-340/0008-kernel-5.16.patch
                ./nvidia-legacy-340/0009-kernel-5.17.patch
                ./nvidia-legacy-340/0010-kernel-5.18.patch
              ];
          });
        };
      nvidia_x11_legacy340 = nvidiaPackages.legacy_340;
    });
  };
in
inputs.nixpkgs.lib.composeManyExtensions [ additions modifications ]
