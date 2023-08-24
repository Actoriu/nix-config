{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) isAttrs;
  inherit (lib) attrValues mkIf mkMerge mkOption;
  inherit (lib.types) nullOr enum;
  # inherit (lib.my) anyAttrs countAttrs value;

  cfg = config.modules.graphical;
in {
  options.modules.graphical = {
    enable = mkEnableOption "Enable support for graphical.";
    display = mkOption {
      type = nullOr (enum ["x11" "wayland"]);
      description = "What display protocol to use.";
      default = null;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      /*
        assertions = [
        {
          assertion = (countAttrs (n: v: n == "enable" && value) cfg) < 2;
          message = "Prevent DE/WM > 1 from being enabled.";
        }
        {
          assertion = let
            srv = config.services;
          in
            srv.xserver.enable
            || srv.sway.enable
            || !(anyAttrs
              (n: v: isAttrs v && anyAttrs (n: v: isAttrs v && v.enable))
              cfg);
          message = "Prevent desktop applications from enabling without a DE/WM.";
        }
      ];
      */

      # system.userActivationScripts.cleanupHome = ''
      #   pushd "${config.user.home}"
      #   rm -rf .compose-cache .nv .pki .dbus .fehbg
      #   [ -s .xsession-errors ] || rm -f .xsession-errors*
      #   popd
      # '';

      services.xserver.enable = true;

      xdg.portal = {
        enable = true;
        extraPortals = with pkgs; [xdg-desktop-portal-gtk];
      };
      services.gnome.gnome-keyring.enable = true;
    }

    (mkIf (cfg.display == "wayland") {
      xdg.portal.wlr.enable = true;

      # Login Manager: ReGreet!
      programs.regreet.enable = true;
    })

    (mkIf (cfg.display == "x11") {
      security.pam.services.lightdm.enableGnomeKeyring = true;
      security.pam.services.lightdm-greeters.enableGnomeKeyring = false;

      services.xserver.displayManager.lightdm = {
        enable = true;
        greeters.mini = {
          enable = true;
          # user = config.user.name;
        };
      };
    })
  ]);
}
