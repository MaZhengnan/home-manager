# ~/.config/nushell/config.nu

# ========== 基础设置 ==========
# 启用语法高亮
# $env.config.color_config = (dark_theme)
# $env.config.use_ansi_coloring = true

# # 智能提示配置
# $env.config.completions = {
#   case_sensitive: false   # 大小写不敏感
#   quick: true             # 快速补全
#   partial: true           # 部分匹配
#   algorithm: "fuzzy"      # 模糊搜索
# }

# # 历史配置
# $env.config.history = {
#   max_size: 10000        # 历史记录数量
#   sync_on_enter: true    # 回车时同步历史
#   file_format: "plaintext"
# }

# # ========== 自定义提示符 ==========
# # 简洁实用的提示符：[用户名@主机名 目录] >
# $env.PROMPT_COMMAND = { ||
#   let user = (whoami | str trim)
#   let host = (sys host | get name | str trim)
#   let path = ($env.PWD | str replace $env.HOME "~")
  
#   $"(ansi green)($user)(ansi reset)@(ansi yellow)($host)(ansi reset) (ansi blue)($path)(ansi reset)"
# }

# $env.PROMPT_COMMAND_RIGHT = { || "" }
# $env.PROMPT_INDICATOR = "〉 "
# $env.PROMPT_INDICATOR_VI_INSERT = ": "
# $env.PROMPT_INDICATOR_VI_NORMAL = "〉 "

# # ========== 实用别名 ==========
# alias ll = ls -l
# alias la = ls -a
# # alias lt = ls --tree
# # alias ltr = ls --tree --full-paths
# alias grep = grep --color=auto
# alias n = nvim
# alias v = nvim
# alias g = git
# alias c = clear
# alias .. = cd ..
# alias ... = cd ../..
# alias h = history
# $env.HM_COMMAND = "home-manager switch --flake /home/mzn/.config/home-manager#mzn"
# alias hm = ^$env.HM_COMMAND
# # alias hm = (home-manager switch --flake /home/mzn/.config/home-manager#mzn)
# # alias hm = $"home-manager switch --flake /home/mzn/.config/home-manager#mzn"
# alias update = sudo apt update; sudo apt upgrade -y   # Ubuntu 专用
# alias py = python3

# # ========== 自定义命令 ==========
# # 显示目录大小（需要安装 du-dust）
# def dus [] {
#   let target = if ($in | is-empty) { "." } else { $in }
#   du-dust -d 1 $target
# }

# # 快速查找文件
# def f [] {
#   let query = if ($in | is-empty) { "" } else { $in }
#   fd --type f $query
# }

# # 快速查找目录
# def fd-dir [] {
#   let query = if ($in | is-empty) { "" } else { $in }
#   fd --type d $query
# }

# # 进程查找
# # def psg [query: string] {
# #   ps aux | grep -i $query
# # }

# # 快速进入常用目录
# def cdd [] {
#   let target = $in
#   if $target == "hm" {
#     cd ~/.config/home-manager
#   } else if $target == "dots" {
#     cd ~/.dotfiles
#   } else if $target == "dl" {
#     cd ~/Downloads
#   } else if $target == "docs" {
#     cd ~/Documents
#   } else {
#     echo "未知目录快捷方式"
#     echo "可用: hm, dots, dl, docs"
#   }
# }

# # ========== 欢迎信息 ==========
# # def welcome [] {
# #   let time = (date now | format date "%H:%M")
# #   let os = (sys | get host.name)
# #   let user = (whoami)
  
# #   print $"(ansi green_bold)欢迎回来, ($user)!(ansi reset)"
# #   print $"(ansi cyan)时间: ($time) | 系统: ($os)(ansi reset)"
# #   print $"(ansi yellow)输入 'help aliases' 查看可用别名(ansi reset)"
# # }

# # # 只在交互模式下显示欢迎信息
# # if (not $env.NU_LOGIN) {
# #   $env.NU_LOGIN = true
# #   welcome
# # }

# # ========== 快捷键提示 ==========
# #
 #print $"(ansi dim)提示: Ctrl+R 搜索历史 | Ctrl+D 退出 | Tab 补全(ansi reset)"