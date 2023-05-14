{
  # editors
  emacs = import ./editors/emacs;
  neovim = import ./editors/neovim;
  # lang
  cc = import ./lang/cc;
  javascript = import ./lang/javascript;
  python = import ./lang/python;
  texlive = import ./lang/texlive;
  # locale
  # locale = import ./locale;
  # misc
  xdg = import ./misc/xdg;
  xresources = import ./misc/xresources;
  # non-nixos
  non-nixos = import ./non-nixos;
  # readers
  zathura = import ./readers/zathura;
  # services
  lorri = import ./services/lorri;
  redshift = import ./services/redshift;
  # shell
  bat = import ./shell/bat;
  dircolors = import ./shell/dircolors;
  direnv = import ./shell/direnv;
  fzf = import ./shell/fzf;
  git = import ./shell/git;
  gnupg = import ./shell/gnupg;
  openssh = import ./shell/openssh;
  password-store = import ./shell/password-store;
  rofi = import ./shell/rofi;
  tmux = import ./shell/tmux;
  zoxide = import ./shell/zoxide;
  zsh = import ./shell/zsh;
  # terminal
  alacritty = import ./terminal/alacritty;
  urxvt = import ./terminal/urxvt;
  xst = import ./terminal/xst;
  # video
  mpv = import ./video/mpv;
}
