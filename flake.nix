{
  description = "Nix configuration with flakes";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    substituters = [
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
      "https://mirrors.ustc.edu.cn/nix-channels/store"
    ];
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://nix-actions.cachix.org"
      "https://nix-on-droid.cachix.org"
      "https://nixos-cn.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-actions.cachix.org-1:WTp8/9EIjoPRzwSERLLMHzDUVGthajaIJ/zEZY6DHvM="
      "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
      "nixos-cn.cachix.org-1:L0jEaL6w7kwQOPlLoCR3ADx+E3Q8SEFEcB9Jaibl0Xg="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";

    nix-formatter-pack = {
      url = "github:Gerschtli/nix-formatter-pack";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    # guix-overlay = {
    #   url = "github:foo-dogsquared/nix-overlay-guix";
    #   inputs = {
    #     nixpkgs.follows = "nixpkgs";
    #   };
    # };

    impermanence.url = "github:nix-community/impermanence";

    nixos-cn = {
      url = "github:nixos-cn/flakes";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
      inputs = {
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };

    # nixos-generators = {
    #   url = "github:nix-community/nixos-generators";
    #   inputs = {
    #     nixpkgs.follows = "nixpkgs";
    #   };
    # };

    nur.url = "github:nix-community/NUR";

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    spacemacs = {
      url = "github:syl20bnr/spacemacs/develop";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , nix-on-droid
    , nix-formatter-pack
    , ...
    }@inputs:
    let
      inherit (self) outputs;

      forEachSystem = nixpkgs.lib.genAttrs [ "aarch64-linux" "x86_64-linux" ];

      lib = nixpkgs.lib.extend (final: prev: {
        my = import ./lib {
          inherit inputs;
          lib = final;
        };
      });

      formatterPackArgsFor = forEachSystem (system: {
        inherit nixpkgs system;
        checkFiles = [ ./. ];

        config.tools = {
          deadnix = {
            enable = true;
            noLambdaPatternNames = true;
          };
          nixpkgs-fmt.enable = true;
          statix.enable = true;
        };
      });
    in {
      # lib = lib.my;

      overlays = {
        # default = import ./overlays { inherit inputs; };
        devshell = inputs.devshell.overlay;
        nixos-cn = inputs.nixos-cn.overlay;
        nur = inputs.nur.overlay;
        sops-nix = inputs.sops-nix.overlay;
        spacemacs = final: prev: { spacemacs = inputs.spacemacs; };
      };

      legacyPackages = forEachSystem (system:
        import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            allowBroken = true;
            allowUnsupportedSystem = true;
          };
          overlays = builtins.attrValues self.overlays;
        });

      checks = forEachSystem (system: {
        nix-formatter-pack-check = nix-formatter-pack.lib.mkCheck formatterPackArgsFor.${system};
      });

      formatter = forEachSystem (system:
        nix-formatter-pack.lib.mkFormatter formatterPackArgsFor.${system});

      # packages = forEachSystem (system:
      #   import ./pkgs { pkgs = self.legacyPackages.${system}; }
      # );

      devShells = forEachSystem (system:
        let
          pkgs = self.legacyPackages.${system};
        in {
          default =
            # let
            #   pkgs = import nixpkgs {
            #     inherit system;
            #     overlays = [ inputs.devshell.overlay ];
            #   };
            # in
            import ./shell/devshell.nix { inherit pkgs; };
        });

      nixosConfigurations = {
        d630 = lib.my.mkNixosConfig {
          extraModules = [
            ({ ... }: {
              nixpkgs = {
                inherit (self.legacyPackages."x86_64-linux") config overlays;
              };
            })
            inputs.impermanence.nixosModules.impermanence
            inputs.nixos-cn.nixosModules.nixos-cn-registries
            inputs.nixos-cn.nixosModules.nixos-cn
            inputs.sops-nix.nixosModules.sops
            ./profiles/nixos
          ];
        };
      };

      #   nixosConfigurations = {
      #     d630 = nixpkgs.lib.nixosSystem {
      #       specialArgs = { inherit inputs outputs; };
      #       modules = [
      #         ({ ... }: {
      #           nixpkgs = {
      #             inherit (self.legacyPackages."x86_64-linux") config overlays;
      #           };
      #         })
      #         inputs.impermanence.nixosModules.impermanence
      #         inputs.nixos-cn.nixosModules.nixos-cn-registries
      #         inputs.nixos-cn.nixosModules.nixos-cn
      #         inputs.sops-nix.nixosModules.sops
      #         inputs.home-manager.nixosModules.home-manager
      #         {
      #           home-manager = {
      #             useGlobalPkgs = true;
      #             useUserPackages = true;
      #             extraSpecialArgs = { inherit inputs self; };
      #             users.actoriu = { ... }: {
      #               home.stateVersion = "22.11";
      #               programs.home-manager.enable = true;
      #               manual.manpages.enable = false;
      #               systemd.user.startServices = "sd-switch";
      #               imports = [
      #                 inputs.impermanence.nixosModules.home-manager.impermanence
      #                 ./modules/home-manager
      #                 ./users/actoriu
      #               ];
      #             };
      #           };
      #         }
      #         ./modules/nixos
      #         ./profiles/nixos
      #         ./hosts/d630
      #       ];
      #     };
      #   };

      #   homeConfigurations = {
      #     "actoriu@d630" = home-manager.lib.homeManagerConfiguration {
      #       # pkgs = nixpkgs.legacyPackages."x86_64-linux";
      #       pkgs = self.legacyPackages."x86_64-linux";
      #       extraSpecialArgs = { inherit inputs self; };
      #       modules = [
      #         ({ ... }: {
      #           nixpkgs = {
      #             inherit (self.legacyPackages."x86_64-linux") config overlays;
      #           };
      #         })
      #         inputs.impermanence.nixosModules.home-manager.impermanence
      #         {
      #           home = {
      #             username = "actoriu";
      #             homeDirectory = "/home/actoriu";
      #             stateVersion = "22.11";
      #           };
      #           programs.home-manager.enable = true;
      #           manual.manpages.enable = false;
      #           systemd.user.startServices = "sd-switch";
      #         }
      #         ./modules/home-manager
      #         ./users/actoriu
      #       ];
      #     };
      #   };

      #   nixOnDroidConfigurations = {
      #     oneplus5 = nix-on-droid.lib.nixOnDroidConfiguration {
      #       pkgs = import nixpkgs {
      #         system = "aarch64-linux";
      #         # inherit (self.legacyPackages."aarch64-linux") config;
      #         overlays = (builtins.attrValues self.overlays) ++ [
      #           nix-on-droid.overlays.default
      #         ];
      #       };
      #       extraSpecialArgs = { inherit inputs self; };
      #       home-manager-path = home-manager.outPath;
      #       modules = [ ./hosts/oneplus5 ];
      #     };
      #   };
    };
}
