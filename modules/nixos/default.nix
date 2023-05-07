{
  fonts = import ./fonts;
  # hardware
  audio = import ./hardware/audio;
  bluetooth = import ./hardware/bluetooth;
  amd = import ./hardware/cpu/amd;
  intel = import ./hardware/cpu/intel;
  opengl = import ./hardware/opengl;
  printers = import ./hardware/printers;
  nvidia = import ./hardware/video/nvidia;
  loader = import ./loader;
  locale = import ./locale;
  network = import ./network;
  power-management = import ./power-management;
  users = import ./users;
}
