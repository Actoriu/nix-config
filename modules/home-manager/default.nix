{
  # editors
  emacs = import ./editors/emacs.nix;
  neovim = import ./editors/neovim.nix;
  # lang
  cc = import ./lang/cc.nix;
  javascript = import ./lang/javascript.nix;
  python = import ./lang/python.nix;
  texlive = import ./lang/texlive.nix;
  # misc
  xresources = import ./misc/xresources.nix;
  xdg = import ./misc/xdg.nix;
  # readers
  zathura = import ./readers/zathura.nix;
  # services
  lorri = import ./services/lorri.nix;
  redshift = import ./services/redshift.nix;
  # shell
  bat = import ./shell/bat.nix;
  dircolors = import ./shell/dircolors.nix;
  direnv = import ./shell/direnv.nix;
  fzf = import ./shell/fzf.nix;
  git = import ./shell/git.nix;
  gnupg = import ./shell/gnupg.nix;
  openssh = import ./shell/openssh.nix;
  password-store = import ./shell/password-store.nix;
  rofi = import ./shell/rofi.nix;
  zoxide = import ./shell/zoxide.nix;
  zsh = import ./shell/zsh.nix;
  # terminal
  alacritty = import ./terminal/alacritty.nix;
  urxvt = import ./terminal/urxvt.nix;
  xst = import ./terminal/xst.nix;
  # video
  mpv = import ./video/mpv.nix;
}
