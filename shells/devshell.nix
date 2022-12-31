{ pkgs
, ...
}:

pkgs.devshell.mkShell {
  name = "nix-config";
  imports = [ (pkgs.devshell.extraModulesDir + "/git/hooks.nix") ];
  git.hooks.enable = true;
  git.hooks.pre-commit.text = "${pkgs.treefmt}/bin/treefmt";
  packages = with pkgs; [
    cachix
    nixpkgs-fmt
    nodePackages.prettier
    nodePackages.prettier-plugin-toml
    shfmt
    treefmt
  ];
  commands = [
    {
      category = "update";
      name = pkgs.nvfetcher.pname;
      help = pkgs.nvfetcher.meta.description;
      command = "cd $PRJ_ROOT/pkgs; ${pkgs.nvfetcher}/bin/nvfetcher -c ./sources.toml $@";
    }
  ];
  devshell.startup.nodejs-setuphook = pkgs.lib.stringsWithDeps.noDepEntry ''
    export NODE_PATH=${pkgs.nodePackages.prettier-plugin-toml}/lib/node_modules:$NODE_PATH
  '';
}
