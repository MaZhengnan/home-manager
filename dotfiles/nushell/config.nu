# ============================================
# Nushell 基础配置文件
# ============================================

# ========== 基础设置 ==========
$env.config = {
    show_banner: false  # 禁用启动横幅
    edit_mode: vi       # 使用 vi 编辑模式
 	# shell_integration: {
    #     osc7: true
    #
	# }
    use_ansi_coloring: true
}

# ========== 环境变量 ==========
$env.EDITOR = "nvim"
$env.VISUAL = "nvim"

# ========== 基本别名 ==========
alias ls = eza
alias ll = eza -l
alias la = eza -a
alias lla = eza -la
alias lt = eza -T
# ========== bat 增强的别名 ==========
alias cat = bat --paging=never
alias catp = bat --paging=always
alias catt = bat --theme=TwoDark
alias catl = bat --list-themes
alias catn = bat --show-all --plain
alias less = bat --paging=always
alias more = bat --paging=always
alias .. = cd ..
alias ... = cd ../..
alias .... = cd ../../..
alias clr = clear
alias emacs = ^bash -c "emacs & disown"
# Home Manager 相关
alias hm = home-manager switch --flake "/home/mzn/.config/home-manager#mzn"

# 系统更新
alias update = sudo apt update; sudo apt upgrade -y

# ========== Starship 提示符配置 ==========
# 确保已安装 starship: nix profile install nixpkgs#starship
$env.STARSHIP_SHELL = "nu"

# 使用 starship 作为提示符
$env.PROMPT_COMMAND = { ||
    starship prompt --cmd-duration $env.CMD_DURATION_MS --status $env.LAST_EXIT_CODE
}

$env.PROMPT_COMMAND_RIGHT = ""

# ========== 自定义函数 ==========
# 显示帮助信息
def help-me [] {
    print (ansi green) "=== 可用别名 ===" (ansi reset)
    print ""
    print "ll, la, lla    - 文件列表"
    print ".., ...        - 目录导航"
    print "c              - 清屏"
    print "h              - 历史命令"
    print "n, v           - 打开 neovim"
    print "g              - git"
    print "hm             - 更新 home-manager"
    print "update         - 系统更新"
    print "py             - python3"
    print ""
    print (ansi yellow) "提示: 已启用 starship 提示符" (ansi reset)
}


# 编辑配置文件
def nucfg [] {
    nvim ~/.config/nushell/config.nu
}

# ========== 启动信息 ==========
#print (ansi green) "✓ Nushell 已加载" (ansi reset)
#print (ansi dim) "输入 'help-me' 查看可用命令" (ansi reset)
