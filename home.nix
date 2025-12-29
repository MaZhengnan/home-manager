{ config, pkgs, lib, ... }:

{
  imports = [
    ./modules/dotfiles.nix  # 导入自动链接模块
  ];

  home.username = "mzn";
  home.homeDirectory = "/home/mzn";
  home.stateVersion = "23.11";
  fonts.fontconfig.enable = true;
  #home.shell = pkgs.nushell;
  #home.shell = "nu";
  #home.shell.NIX_MAIN_PROGRAM = pkgs.nushell;
  programs.nushell.enable = true;
  # 只管理软件包安装
  home.packages = with pkgs; [
    kitty
	alacritty
	ghostty
	# conky
	# dunst
	# eww
	# pcmanfm
	neovim
    git
    nushell
    emacs
	# polybar
	# picom
	# rofi
    nerd-fonts.jetbrains-mono
    wqy_microhei
  ];
  targets.genericLinux.enable = true;
  
  # 自动复制 nix.conf 到正确位置
  home.file.".config/nix/nix.conf".source = ./nix.conf;
  
  # Home Manager 基础设置
  programs.home-manager.enable = true;
}
