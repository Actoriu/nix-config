{...}: {
  imports = [
    ./editors/emacs.nix
    ./misc/genericLinux.nix
    ./misc/graphical.nix
    ./programs/npm.nix
    ./terminal/alacritty.nix
    # ./termianl/xst.nix
  ];
}
