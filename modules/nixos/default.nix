{
  amd = import ./amd.nix;
  audio = import ./audio.nix;
  bluetooth = import ./bluetooth;
  fonts = import ./fonts.nix;
  intel = import ./intel.nix;
  loader = import ./loader.nix;
  locale = import ./locale.nix;
  network = import ./network.nix;
  nvidia = import ./nvidia.nix;
  opengl = import ./opengl.nix;
  power-management = import ./power-management.nix;
  printers = import ./printers.nix;
  users = import ./users.nix;
}
