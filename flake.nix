{
  description = "My complete Nix environment (stable + unstable)";

  inputs = {
    # 稳定底座
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

    # 只用于新软件
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Home Manager（和 stable 对齐）
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, ... }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

      pkgsUnstable = import nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
      };
    in {
      homeConfigurations.mzn =
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          # 👇 把 unstable 显式传给 home.nix
          extraSpecialArgs = {
            inherit pkgsUnstable;
          };

          modules = [
            ./home.nix
          ];
        };

      # 可选：开发用 shell
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          git
          home-manager
        ];
        shellHook = ''
          echo "📦 Nix stable (24.05) + unstable environment"
        '';
      };
    };
}