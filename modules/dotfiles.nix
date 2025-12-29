{ config, lib, ... }:

let
  # dotfiles 目录的绝对路径
  dotfilesDir = "${config.home.homeDirectory}/.config/home-manager/dotfiles";
  
  # 配置映射：目标路径 -> 源路径（相对于 dotfilesDir）
  configMap = {
    ".config/nvim" = "nvim";
    ".config/i3" = "i3wm/i3";
    ".config/conky" = "i3wm/conky";
    ".config/eww" = "i3wm/eww";
    ".config/dunst" = "i3wm/dunst";
    ".config/pcmanfm" = "i3wm/pcmanfm";
    ".config/rofi" = "i3wm/rofi";
    ".config/polybar" = "i3wm/polybar";
    ".config/picom" = "i3wm/picom";
    ".config/nushell" = "nushell";
    ".emacs.d" = "emacs";
    ".gitconfig" = "git/.gitconfig";
    ".bashrc" = "shell/.bashrc";
    # 添加你的其他配置...
  };

  # 创建符号链接的 helper 函数
  mkLink = target: source:
    let
      sourcePath = "${dotfilesDir}/${source}";
      isDir = builtins.pathExists sourcePath && 
              (let attrs = builtins.readDir sourcePath;
               in attrs ? "default.nix" || (builtins.length (builtins.attrNames attrs)) > 0);
    in {
      "${target}" = {
        source = config.lib.file.mkOutOfStoreSymlink sourcePath;
        recursive = isDir;
      };
    };

in {
  home.file = lib.foldl' (acc: elem: acc // elem) {} 
    (lib.mapAttrsToList mkLink configMap);
}
