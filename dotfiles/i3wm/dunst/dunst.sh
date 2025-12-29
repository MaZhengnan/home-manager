#!/bin/bash
# Dunst 控制脚本

case $1 in
    "pause")
        dunstctl set-paused true
        notify-send "通知暂停" "新通知将被暂存"
        ;;
    "resume")
        dunstctl set-paused false
        notify-send "通知恢复" "开始显示通知"
        ;;
    "close")
        dunstctl close
        ;;
    "close-all")
        dunstctl close-all
        ;;
    "history")
        dunstctl history-pop
        ;;
    "test")
        ~/.config/dunst/test-notify.sh
        ;;
    *)
        echo "用法: $0 {pause|resume|close|close-all|history|test}"
        ;;
esac
