{
  description = "My complete Nix environment with dotfiles";

  inputs = {
    # 或者使用最新的 unstable
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    
    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs"; # 确保使用相同的 nixpkgs
  };
  outputs = { self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations.mzn = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ 
          ./home.nix 
          # 可以在这里添加其他模块
        ];
      };
      
      # 开发环境（可选）
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [ git home-manager ];
        shellHook = ''
          echo "📦 Nix environment with dotfiles"
        '';
      };
    };
}
