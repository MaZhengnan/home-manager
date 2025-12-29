#!/bin/bash
export DISPLAY=:0
export MONITOR=Virtual1

# 快速清理
pkill -9 polybar 2>/dev/null
sleep 0.1

# 立即启动
polybar -c ~/.config/polybar/config.ini mybar > /tmp/polybar.log 2>&1 &

# 快速验证（不等待）
sleep 0.3
if ps aux | grep -q "[p]olybar"; then
    echo "✅ Polybar 已启动"
else
    echo "❌ 启动失败，查看 /tmp/polybar.log"
fi
