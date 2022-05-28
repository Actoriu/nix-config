{
  extraModulesPath,
  inputs,
  lib,
  pkgs,
  ...
}: let
  # hooks = import ./hooks;

  pkgWithCategory = category: package: { inherit package category; };
  devos = pkgWithCategory "devos";
  docs = pkgWithCategory "docs";
  linter = pkgWithCategory "linter";
  secret = pkgWithCategory "secret";

in
  {
    _file = toString ./.;

    imports = [ "${extraModulesPath}/git/hooks.nix" ];
    # git = { inherit hooks; };
    git.hooks.enable = true;
    git.hooks.pre-commit.text = "${pkgs.treefmt}/bin/treefmt";

    # tempfix: remove when merged https://github.com/numtide/devshell/pull/123
    devshell.startup.load_profiles = pkgs.lib.mkForce (pkgs.lib.noDepEntry ''
    # PATH is devshell's exorbitant privilige:
    # fence against its pollution
    _PATH=''${PATH}
    # Load installed profiles
    for file in "$DEVSHELL_DIR/etc/profile.d/"*.sh; do
      # If that folder doesn't exist, bash loves to return the whole glob
      [[ -f "$file" ]] && source "$file"
    done
    # Exert exorbitant privilige and leave no trace
    export PATH=''${_PATH}
    unset _PATH
  '');

    commands = with pkgs; [
      (devos nixUnstable)
      (devos nix-build-uncached)
      (devos inputs.deploy-rs.packages.${pkgs.system}.deploy-rs)

      {
        category = "devos";
        name = pkgs.nvfetcher-bin.pname;
        help = pkgs.nvfetcher-bin.meta.description;
        command = "cd $PRJ_ROOT/pkgs; ${pkgs.nvfetcher-bin}/bin/nvfetcher -c ./sources.toml $@";
      }

      # (linter nixpkgs-fmt)
      # (linter editorconfig-checker)

      (docs mdbook)
    ]
    ++ lib.optional (!pkgs.stdenv.buildPlatform.isi686)
      (devos cachix)
    ++ lib.optional (pkgs.stdenv.hostPlatform.isLinux && !pkgs.stdenv.buildPlatform.isDarwin)
      (devos inputs.nixos-generators.defaultPackage.${pkgs.system})
    ;
  }
