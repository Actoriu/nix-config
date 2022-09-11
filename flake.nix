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
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = {
        flake-compat.follows = "flake-compat";
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

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    /*
      emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
      flake-utils.follows = "flake-utils-plus/flake-utils";
      nixpkgs.follows = "nixos";
      };
      };

      guix-overlay = {
      url = "github:foo-dogsquared/nix-overlay-guix";
      inputs = {
      nixpkgs.follows = "latest";
      };
      };
    */

    impermanence.url = "github:nix-community/impermanence";

    /*
      neovim-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs = {
      flake-compat.follows = "flake-compat";
      nixpkgs.follows = "latest";
      };
      };
    */

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
        flake-utils.follows = "flake-utils";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };

    /*
      nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs = {
      nixpkgs.follows = "nixpkgs";
      };
      };
    */

    nur.url = "github:nix-community/NUR";

    nvfetcher = {
      url = "github:berberman/nvfetcher";
      inputs = {
        flake-compat.follows = "flake-compat";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    peerix = {
      url = "github:misterio77/peerix";
      inputs = {
        flake-compat.follows = "flake-compat";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    /*
      ragenix = {
      url = "github:yaxitech/ragenix";
      inputs = {
      flake-utils.follows = "flake-utils-plus/flake-utils";
      nixpkgs.follows = "latest";
      };
      };
    */

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

  outputs = { self, ... }@inputs:
    let
      lib = import ./lib {
        inherit inputs;
      };
      inherit (lib) mkSystem mkHome mkDroid mkDeploys eachDefaultSystem;
    in
    rec {
      inherit lib;

      overlays = {
        default = import ./overlays { inherit inputs; };
        devshell = inputs.devshell.overlay;
        nixos-cn = inputs.nixos-cn.overlay;
        nur = inputs.nur.overlay;
        nvfetcher = inputs.nvfetcher.overlay;
        peerix = inputs.peerix.overlay;
        sops-nix = inputs.sops-nix.overlay;
      };

      legacyPackages = eachDefaultSystem (system:
        import inputs.nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            allowBroken = true;
            allowUnsupportedSystem = true;
          };
          overlays =
            builtins.attrValues overlays ++ [
              (final: prev: { spacemacs = inputs.spacemacs; })
            ];
        }
      );

      formatter = eachDefaultSystem (system: legacyPackages.${system}.nixpkgs-fmt);

      apps = eachDefaultSystem (system: rec {
        deploy = {
          type = "app";
          program = "${legacyPackages.${system}.deploy-rs}/bin/deploy";
        };
        default = deploy;
      });

      devShells = eachDefaultSystem (system:
        let
          pkgs = legacyPackages.${system};
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
        });

      nixosConfigurations = {
        # Laptop
        d630 = mkSystem {
          hostname = "d630";
          username = "actoriu";
          pkgs = legacyPackages."x86_64-linux";
          persistence = true;
          extraModules = [ ./users/modules ];
        };
      };

      homeConfigurations = {
        # Laptop
        "actoriu@d630" = mkHome {
          username = "actoriu";
          hostname = "d630";
          persistence = true;
        };
      };

      nixOnDroidConfigurations = {
        oneplus5 = mkDroid {
          devicename = "oneplus5";
          pkgs = legacyPackages."aarch64-linux";
          persistence = false;
        };
      };

      deploy = {
        nodes = mkDeploys nixosConfigurations homeConfigurations;
        magicRollback = false;
        autoRollback = false;
      };

      deployChecks = { };
    };

  /*
    let
    inherit (flake-utils-plus.lib) mkFlake exportModules exportPackages exportOverlays;
    in
    mkFlake
    {
    inherit self inputs;

    supportedSystems = [ "aarch64-linux" "x86_64-linux" ];

    channelsConfig = { allowUnfree = true; };

    overlay = import ./overlays;

    overlays = exportOverlays {
    inherit (self) pkgs inputs;
    };

    sharedOverlays = [
    self.overlay
    devshell.overlay
    nixos-cn.overlay
    nur.overlay
    nvfetcher.overlay
    (final: prev: { spacemacs = inputs.spacemacs; })
    ];

    hostDefaults = {
    channelName = "nixos";
    # modules = [ ];
    specialArgs = { inherit inputs; };
    };

    hosts = {
    d630 = {
    system = "x86_64-linux";
    modules = [
    impermanence.nixosModules.impermanence
    nixos-cn.nixosModules.nixos-cn-registries
    nixos-cn.nixosModules.nixos-cn
    home.nixosModules.home-manager
    {
    home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.actoriu = { config, lib, pkgs, ... }: {
    imports = [
    # impermanence.nixosModules.home-manager.impermanence
    ./users/actoriu
    ];
    };
    };
    }
    # ({ pkgs, ... }: {
    #   system.configurationRevision =
    #     nixos.lib.mkIf (self ? rev) self.rev;
    # })
    ./hosts/d630
    ];
    };

    oneplus5 = {
    system = "aarch64-linux";
    modules = [
    ./hosts/oneplus5
    ];
    output = "nixOnDroidConfigurations";
    builder = { system, modules, ... }:
    nix-on-droid.lib.nixOnDroidConfiguration {
    inherit system;
    config = { imports = modules; };
    pkgs = import nixos {
    inherit system;
    overlays = [ (final: prev: { spacemacs = inputs.spacemacs; }) ];
    };
    };
    };
    };

    outputsBuilder = channels:
    let
    pkgs = channels.nixos;
    in
    {
    devShells =
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
    packages = exportPackages self.overlays channels;
    };
    };
    // (
    let
    system = "x86_64-linux";
    pkgs = nixos.legacyPackages.${system};
    in
    {
    homeConfigurations = import ./machines/home/default.nix { inherit self inputs pkgs; };
    }
    );
  */
}
