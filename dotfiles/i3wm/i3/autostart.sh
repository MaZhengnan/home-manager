# #!/bin/bash
# # i3 自动启动脚本

# # 等待 i3 完全就绪
# sleep 1
# feh --bg-scale ~/.config/home-manager/dotfiles/i3wm/wallpapers/nord.jpeg &

# # 合成器（透明、阴影效果）
# picom --config ~/.config/picom/picom.conf -b &

# # 状态栏
# ~/.config/polybar/launch.sh &

# # # 网络管理器托盘
# # nm-applet &

# # # 输入法
# # fcitx5 -d &

# # # 剪贴板管理器
# # copyq &

# # # 壁纸
# # feh --bg-scale ~/Pictures/Wallpapers/default.jpg &

# # # 其他应用
# # thunderbird &
# # discord &

# echo "自动启动完成"


#!/bin/bash
# i3 完整自动启动脚本
# 保存日志到文件便于调试

LOG_FILE="/tmp/i3-autostart-$(date +%s).log"
echo "=== i3 自动启动 $(date) ===" > $LOG_FILE

# 等待 X 服务器就绪
sleep 1
export DISPLAY=:0
echo "DISPLAY: $DISPLAY" >> $LOG_FILE

# ========== 1. 壁纸设置 ==========
echo "设置壁纸..." >> $LOG_FILE
WALLPAPER="$HOME/.config/home-manager/dotfiles/i3wm/wallpapers/nord.jpeg"
if [ -f "$WALLPAPER" ]; then
    feh --bg-scale "$WALLPAPER" >> $LOG_FILE 2>&1 &
    echo "✅ 壁纸设置: $WALLPAPER" >> $LOG_FILE
else
    # 备用：纯色背景
    xsetroot -solid "#2f343f" >> $LOG_FILE 2>&1 &
    echo "⚠️  使用纯色背景（壁纸不存在）" >> $LOG_FILE
fi

# ========== 2. 合成器（透明/阴影）==========
echo "启动 Picom..." >> $LOG_FILE
if pgrep picom >/dev/null; then
    pkill picom
    sleep 0.5
fi
picom --config ~/.config/picom/picom.conf -b >> $LOG_FILE 2>&1 &
echo "✅ Picom 已启动" >> $LOG_FILE

# ========== 3. 状态栏 ==========
echo "启动 Polybar..." >> $LOG_FILE
sleep 1  # 等待其他组件就绪
if [ -x ~/.config/polybar/launch.sh ]; then
    ~/.config/polybar/launch.sh >> $LOG_FILE 2>&1 &
    echo "✅ Polybar 已启动" >> $LOG_FILE
else
    echo "❌ Polybar 启动脚本不存在或不可执行" >> $LOG_FILE
fi

# ========== 4. 通知系统 ==========
echo "启动 Dunst..." >> $LOG_FILE
if pgrep dunst >/dev/null; then
    pkill dunst
    sleep 0.5
fi
dunst --config ~/.config/dunst/dunstrc >> $LOG_FILE 2>&1 &
echo "✅ Dunst 已启动" >> $LOG_FILE

# ========== 5. 网络管理器 ==========
echo "启动网络管理器..." >> $LOG_FILE
if command -v nm-applet >/dev/null; then
    nm-applet >> $LOG_FILE 2>&1 &
    echo "✅ 网络管理器已启动" >> $LOG_FILE
fi

# ========== 6. 输入法（可选）==========
# echo "启动输入法..." >> $LOG_FILE
# if command -v fcitx5 >/dev/null; then
#     fcitx5 -d >> $LOG_FILE 2>&1 &
#     echo "✅ 输入法已启动" >> $LOG_FILE
# fi

# ========== 7. 蓝牙管理器（可选）==========
# echo "启动蓝牙管理器..." >> $LOG_FILE
# if command -v blueman-applet >/dev/null; then
#     blueman-applet >> $LOG_FILE 2>&1 &
#     echo "✅ 蓝牙管理器已启动" >> $LOG_FILE
# fi

# ========== 8. 剪贴板管理器（可选）==========
# echo "启动剪贴板管理器..." >> $LOG_FILE
# if command -v copyq >/dev/null; then
#     copyq >> $LOG_FILE 2>&1 &
#     echo "✅ 剪贴板管理器已启动" >> $LOG_FILE
# fi

# ========== 9. 电源管理器（可选）==========
# echo "启动电源管理器..." >> $LOG_FILE
# if command -v xfce4-power-manager >/dev/null; then
#     xfce4-power-manager >> $LOG_FILE 2>&1 &
#     echo "✅ 电源管理器已启动" >> $LOG_FILE
# fi

# ========== 10. 声音托盘（可选）==========
# echo "启动声音托盘..." >> $LOG_FILE
# if command -v pasystray >/dev/null; then
#     pasystray >> $LOG_FILE 2>&1 &
#     echo "✅ 声音托盘已启动" >> $LOG_FILE
# fi

# ========== 11. Rofi 配置检查 ==========
echo "检查 Rofi 配置..." >> $LOG_FILE
if [ ! -f ~/.config/rofi/themes/rounded-blue.rasi ]; then
    echo "⚠️  Rofi 主题不存在，使用默认" >> $LOG_FILE
else
    echo "✅ Rofi 主题已就绪" >> $LOG_FILE
fi

# ========== 发送完成通知 ==========
sleep 2
if command -v notify-send >/dev/null; then
    notify-send -t 3000 "i3 桌面" "启动完成 ✓" >> $LOG_FILE 2>&1
fi

echo "自动启动完成！" >> $LOG_FILE
echo "查看日志: $LOG_FILE"

# 显示最后几行日志
tail -5 $LOG_FILE
