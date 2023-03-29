# This file defines overlays
{inputs, ...}: {
  # This one brings our custom packages from the 'pkgs' directory
  additions = final: _prev: import ../pkgs {pkgs = final;};

  # This one contains whatever you want to overlay
  # You can change versions, add patches, set compilation flags, anything really.
  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    linuxPackages = prev.linuxPackages.extend (final: prev: rec {
      nvidiaPackages =
        prev.nvidiaPackages
        // {
          legacy_340 = prev.nvidiaPackages.legacy_340.overrideAttrs (oldAttrs: {
            patches =
              # (oldAttrs.patches or []) ++
              [
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
                ./nvidia-legacy-340/0011-kernel-6.0.patch
                ./nvidia-legacy-340/0012-kernel-6.2.patch
              ];
          });
        };
    });
  };

  # When applied, the spacemacs set (declared in the flake inputs) will
  # be accessible through 'pkgs.spacemacs'
  spacemacs = final: _prev: {
    spacemacs = inputs.spacemacs;
  };
}
