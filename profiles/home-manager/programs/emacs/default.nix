{
  config,
  lib,
  pkgs,
  ...
}:
with lib; {
  home = {
    packages = with pkgs; [
      binutils
      fd
      sqlite
      gnutls
      (ripgrep.override {withPCRE2 = true;})
      (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
      (hunspellWithDicts (with hunspellDicts; [en_GB-large]))
      (nuspellWithDicts (with hunspellDicts; [en_GB-large]))
      (mkIf (config.programs.gpg.enable)
        pinentry-emacs)
      zstd
      # :lang cc
      clang-tools
      ccls
      # :lang python
      nodePackages.pyright
      imagemagick
      guile_3_0
      ugrep
      translate-shell
    ];
  };
}
