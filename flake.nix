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
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-actions.cachix.org-1:WTp8/9EIjoPRzwSERLLMHzDUVGthajaIJ/zEZY6DHvM="
      "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";

    # nix-formatter-pack = {
    #   url = "github:Gerschtli/nix-formatter-pack";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cachix-deploy-flake = {
      url = "github:cachix/cachix-deploy-flake";
      inputs = {
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        flake-compat.follows = "flake-compat";
        flake-utils.follows = "flake-utils";
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

  outputs = {
    self,
    home-manager,
    nixpkgs,
    nix-on-droid,
    # cachix-deploy-flake,
    pre-commit-hooks,
    ...
  } @ inputs: let
    inherit (self) outputs;

    forEachSystem = nixpkgs.lib.genAttrs ["aarch64-linux" "x86_64-linux"];
    forEachPkgs = f: forEachSystem (sys: f nixpkgs.legacyPackages.${sys});

    # cachixDeployLibFor =
    #   forEachSystem (system:
    #     cachix-deploy-flake.lib nixpkgs.legacyPackages.${system});

    version = nixpkgs.lib.fileContents ./.version;
  in {
    overlays = import ./overlays {inherit inputs;};

    checks = forEachSystem (system: {
      pre-commit-check = pre-commit-hooks.lib.${system}.run {
        src = ./.;
        hooks = {
          treefmt.enable = true;
        };
        settings = {
          treefmt.package = nixpkgs.legacyPackages.${system}.treefmt;
        };
      };
    });

    formatter = forEachPkgs (pkgs: pkgs.alejandra);

    packages = forEachPkgs (pkgs: import ./pkgs {inherit pkgs;});

    devShells = forEachSystem (system: {
      default = nixpkgs.legacyPackages.${system}.mkShell {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
      };
    });

    # devShells = forEachSystem (system: {
    #   default = let
    #     pkgs = import nixpkgs {
    #       inherit system;
    #       overlays = [inputs.devshell.overlays.default];
    #     };
    #   in
    #     import ./shell/devshell.nix {inherit pkgs;};
    # });

    nixosConfigurations = {
      d630 = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit inputs outputs version;
          desktop = null;
          hostname = "d630";
          username = "actoriu";
          system = "x86_64-linux";
        };
        modules = [./hosts/nixos/shared];
      };
    };

    homeConfigurations = {
      "actoriu@d630" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."x86_64-linux";
        extraSpecialArgs = {
          inherit inputs outputs version;
          desktop = null;
          hostname = "d630";
          username = "actoriu";
          system = "x86_64-linux";
        };
        modules = [./users/shared];
      };
    };

    nixOnDroidConfigurations = {
      oneplus5 = nix-on-droid.lib.nixOnDroidConfiguration {
        pkgs = import nixpkgs {
          system = "aarch64-linux";
          overlays =
            (builtins.attrValues self.overlays)
            ++ [
              nix-on-droid.overlays.default
            ];
        };
        extraSpecialArgs = {
          inherit inputs outputs version;
          devicename = "oneplus5";
          username = "nix-on-droid";
        };
        home-manager-path = home-manager.outPath;
        modules = [./hosts/droid/oneplus5];
      };
    };
  };
}
