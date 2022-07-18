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

    flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus";

    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        flake-utils.follows = "flake-utils-plus/flake-utils";
        nixpkgs.follows = "latest";
      };
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        utils.follows = "flake-utils-plus/flake-utils";
        nixpkgs.follows = "latest";
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
        flake-utils.follows = "flake-utils-plus/flake-utils";
        nixpkgs.follows = "latest";
      };
    };

    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
      inputs = {
        flake-utils.follows = "flake-utils-plus/flake-utils";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "latest";
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
        flake-utils.follows = "flake-utils-plus/flake-utils";
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

  outputs = { self, ... }@inputs:
    inputs.flake-utils-plus.lib.mkFlake {
      inherit self inputs;

      supportedSystems = [ "aarch64-linux" "x86_64-linux" ];

      channelsConfig = { allowUnfree = true; };

      sharedOverlays = [
        inputs.devshell.overlay
        inputs.nixos-cn.overlay
        inputs.nur.overlay
        inputs.nvfetcher.overlay
        (final: prev: { spacemacs = inputs.spacemacs; })
      ];

      hostDefaults = {
        channelName = "latest";
        modules = [
          inputs.home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
          ./machines/modules
          ./users/modules
        ];
      };

      hosts = {
        d630 = {
          system = "x86_64-linux";
          modules = [
            inputs.impermanence.nixosModules.impermanence
            inputs.nixos-cn.nixosModules.nixos-cn-registries
            inputs.nixos-cn.nixosModules.nixos-cn
            ./hosts/d630
          ];
        };

        oneplus5 = {
          system = "aarch64-linux";
          output = "nixOnDroidConfigurations";
          builder = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
            config = import ./hosts/oneplus5 { inherit self inputs; };
          };
        };
      };

      outputsBuilder = channels:
        let
          pkgs = channels.latest;
        in
        {
          devShells = {
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
        };
    };
}
