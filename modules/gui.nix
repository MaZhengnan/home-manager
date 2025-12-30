{ pkgs, pkgsUnstable, lib, ... }:

let
  # 小工具：快速生成 desktop entry
  mkGui = {
    name,
    exec,
    icon ? name,
    categories ? [ "Utility" ],
    terminal ? false
  }: {
    "${name}" = {
      inherit name exec icon terminal categories;
    };
  };
in
{
  xdg.desktopEntries =
    lib.mkMerge [

      # ===== Emacs =====
      (mkGui {
        name = "Emacs";
        exec = "${pkgsUnstable.emacs}/bin/emacs %F";
        categories = [ "Development" "TextEditor" ];
        icon = "emacs";
      })

      # ===== Kitty =====
      (mkGui {
        name = "Kitty";
        exec = "${pkgs.kitty}/bin/kitty";
        categories = [ "System" "TerminalEmulator" ];
        icon = "kitty";
      })

      # ===== Alacritty =====
      (mkGui {
        name = "Alacritty";
        exec = "${pkgs.alacritty}/bin/alacritty";
        categories = [ "System" "TerminalEmulator" ];
        icon = "Alacritty";
      })

      # ===== Neovim (GUI wrapper) =====
      (mkGui {
        name = "Neovim";
        exec = "${pkgsUnstable.neovim}/bin/nvim";
        categories = [ "Development" ];
        terminal = true;
        icon = "nvim";
      })
    ];
}