{
  fonts = import ./fonts.nix;
  # hardware
  audio = import ./hardware/audio.nix;
  bluetooth = import ./hardware/bluetooth.nix;
  amd = import ./hardware/cpu/amd.nix;
  intel = import ./hardware/cpu/intel.nix;
  opengl = import ./hardware/opengl.nix;
  printers = import ./hardware/printers.nix;
  nvidia = import ./hardware/video/nvidia.nix;
  loader = import ./loader.nix;
  locale = import ./locale.nix;
  network = import ./network.nix;
  power-management = import ./power-management.nix;
  users = import ./users.nix;
}
