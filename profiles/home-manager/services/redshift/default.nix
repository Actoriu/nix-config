{
  config,
  lib,
  pkgs,
  ...
}: {
  services = {
    redshift = {
      enable = true;
      tray = true;
      temperature = {
        day = 5800;
        night = 4600;
      };
      provider = "manual";
      latitude = 24.1958;
      longitude = 102.9274;
      settings = {
        redshift = {
          transition = 1;
          brightness-day = 0.9;
          brightness-night = 0.7;
          gamma = 0.8;
          adjustment-method = "randr";
        };
      };
    };
  };
}
