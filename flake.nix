{
  description = "Nix configuration with flakes";

  nixConfig = {
    extra-experimental-features = "nix-command flakes recursive-nix";
    substituters = [
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
      "https://mirrors.ustc.edu.cn/nix-channels/store"
    ];
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://nix-actions.cachix.org"
      "https://nix-on-droid.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-actions.cachix.org-1:WTp8/9EIjoPRzwSERLLMHzDUVGthajaIJ/zEZY6DHvM="
      "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
    ];
  };

  inputs = {
    nixos.url = "github:NixOS/nixpkgs/nixos-22.05";
    latest.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";

    # flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus";

    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "latest";
      };
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs = {
        utils.follows = "flake-utils";
        nixpkgs.follows = "nixos";
      };
    };

    # emacs-overlay = {
    #   url = "github:nix-community/emacs-overlay";
    #   inputs = {
    #     flake-utils.follows = "flake-utils-plus/flake-utils";
    #     nixpkgs.follows = "nixos";
    #   };
    # };

    # guix-overlay = {
    #   url = "github:foo-dogsquared/nix-overlay-guix";
    #   inputs = {
    #     nixpkgs.follows = "latest";
    #   };
    # };

    impermanence.url = "github:nix-community/impermanence";

    # neovim-overlay = {
    #   url = "github:nix-community/neovim-nightly-overlay";
    #   inputs = {
    #     flake-compat.follows = "flake-compat";
    #     nixpkgs.follows = "latest";
    #   };
    # };

    nixos-cn = {
      url = "github:nixos-cn/flakes";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "latest";
      };
    };

    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
      inputs = {
        flake-utils.follows = "flake-utils";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixos";
      };
    };

    # nixos-hardware.url = "github:NixOS/nixos-hardware";

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs = {
        nixpkgs.follows = "latest";
      };
    };

    nur.url = "github:nix-community/NUR";

    nvfetcher = {
      url = "github:berberman/nvfetcher";
      inputs = {
        flake-compat.follows = "flake-compat";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "latest";
      };
    };

    # ragenix = {
    #   url = "github:yaxitech/ragenix";
    #   inputs = {
    #     flake-utils.follows = "flake-utils-plus/flake-utils";
    #     nixpkgs.follows = "latest";
    #   };
    # };

    # sops-nix = {
    #   url = "github:Mic92/sops-nix";
    #   inputs = {
    #     nixpkgs.follows = "latest";
    #     nixpkgs-22_05.follows = "nixos";
    #   };
    # };

    spacemacs = {
      url = "github:syl20bnr/spacemacs/develop";
      flake = false;
    };
  };

  outputs = { self, ... }@inputs: {
    overlays.default = import ./overlays;
    nixosConfigurations = import ./machines/nixos/default.nix { inherit self inputs; };
    # nixosConfigurations = {
    #   d630 = inputs.nixos.lib.nixosSystem {
    #     system = "x86_64-linux";
    #     specialArgs = { inherit inputs; };
    #     modules = with inputs; [
    #       impermanence.nixosModules.impermanence
    #       nixos-cn.nixosModules.nixos-cn-registries
    #       nixos-cn.nixosModules.nixos-cn
    #       home-manager.nixosModules.home-manager
    #       ({ pkgs, ... }: {
    #         nixpkgs = {
    #           config = { allowUnfree = true; };
    #           overlays = with inputs; [
    #             nixos-cn.overlay
    #             nur.overlay
    #             nvfetcher.overlay
    #             (final: prev: { spacemacs = inputs.spacemacs; })
    #           ];
    #         };
    #         system.configurationRevision =
    #           inputs.nixos.lib.mkIf (self ? rev) self.rev;
    #       })
    #       ./profiles/shared/home-manager
    #       ./hosts/d630
    #     ];
    #   };
    # };
    nixOnDroidConfigurations = import ./machines/droid/default.nix { inherit self inputs; };
    # nixOnDroidConfigurations = {
    #   oneplus5 = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
    #     system = "aarch64-linux";
    #     config = ./hosts/oneplus5/default.nix;
    #   };
    # };
    # homeConfigurations = {
    #   actoriu = inputs.home-manager.lib.homeManagerConfiguration {
    #     modules = [
    #       ./user/actoriu
    #     ];
    #   };
    # };
  } // {
    (import ./machines/home/default.nix { inherit self inputs; };)
    }
    // inputs.flake-utils.lib.eachSystem [ "aarch64-linux" "x86_64-linux" ] (system:
    {
    devShells =
      let pkgs = import inputs.nixos {
        inherit system;
        overlays = [ inputs.devshell.overlay ];
      };
      in
      {
        default = pkgs.devshell.mkShell {
          name = "nix-config";
          imports = [ (pkgs.devshell.extraModulesDir + "/git/hooks.nix") ];
          git.hooks.enable = true;
          git.hooks.pre-commit.text = "${pkgs.treefmt}/bin/treefmt";
          packages = with pkgs; [
            cachix
            nix-build-uncached
            nixpkgs-fmt
            nodePackages.prettier
            nodePackages.prettier-plugin-toml
            shfmt
            treefmt
          ];
          devshell.startup.nodejs-setuphook = pkgs.lib.stringsWithDeps.noDepEntry ''
            export NODE_PATH=${pkgs.nodePackages.prettier-plugin-toml}/lib/node_modules:$NODE_PATH
          '';
        };
      };
  });
}
