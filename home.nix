{ config, pkgs, pkgsUnstable, lib, ... }:

{
  imports = [
    ./modules/dotfiles.nix
    ./modules/gui.nix
  ];

  home = {
    username = "mzn";
    homeDirectory = "/home/mzn";
    stateVersion = "23.11";
  };

  # ===== XDG & GUI 环境 =====
  xdg.enable = true;

  # GUI 会话 PATH（修 rofi / GNOME 无法启动）
  home.sessionPath = [
    "$HOME/.nix-profile/bin"
    "/nix/var/nix/profiles/default/bin"
  ];

  fonts.fontconfig.enable = true;

  targets.genericLinux.enable = true;

  # ===== 软件安装策略 =====
  home.packages = [
    # ---- 稳定工具 ----
    pkgs.git
    pkgs.nushell
    pkgs.starship
    pkgs.fzf
    pkgs.eza
    pkgs.bat
    # ---- 追新工具（unstable）----
    pkgsUnstable.neovim
    pkgsUnstable.emacs
	pkgsUnstable.wezterm
    # ---- 字体 ----
    pkgsUnstable.nerd-fonts.jetbrains-mono
    pkgs.wqy_microhei
  ];

  # ===== Home Manager 自身 =====
  programs.home-manager.enable = true;
}