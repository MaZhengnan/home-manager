#!/bin/bash
# 文件名: add-permissions.sh
# 用法: ./add-permissions.sh 文件1 文件2 文件3 ...
# 示例: ./add-permissions.sh abc.sh bcd.sh efg.sh

# 预定义文件列表（可以在这里添加更多文件）
DEFAULT_FILES=("~/.config/home-manager/dotfiles/i3wm/i3/autostart.sh" 
               "~/.config/home-manager/dotfiles/i3wm/polybar/launch.sh"
               "~/.config/home-manager/dotfiles/i3wm/dunst/dunst.sh")  # 默认文件列表

echo "🔧 文件权限添加工具"
echo "====================="

# 如果没有传入参数，使用默认文件列表
if [ $# -eq 0 ]; then
    FILES=("${DEFAULT_FILES[@]}")
    echo "使用默认文件列表:"
else
    FILES=("$@")
    echo "使用传入的文件列表:"
fi

# 显示要处理的文件
for file in "${FILES[@]}"; do
    echo "  - $file"
done

echo ""
echo "正在检查文件..."
EXISTING_FILES=()
MISSING_FILES=()

# 检查文件是否存在
for file in "${FILES[@]}"; do
    if [ -f "$file" ]; then
        EXISTING_FILES+=("$file")
    else
        MISSING_FILES+=("$file")
    fi
done

# 显示缺失的文件
if [ ${#MISSING_FILES[@]} -gt 0 ]; then
    echo ""
    echo "⚠️  以下文件不存在:"
    for file in "${MISSING_FILES[@]}"; do
        echo "  - $file"
    done
    
    read -p "是否继续处理存在的文件？(y/N): " CONTINUE
    if [[ ! $CONTINUE =~ ^[Yy]$ ]]; then
        echo "操作取消"
        exit 1
    fi
fi

if [ ${#EXISTING_FILES[@]} -eq 0 ]; then
    echo "❌ 没有可处理的文件"
    exit 1
fi

# 添加权限
echo ""
echo "正在添加执行权限..."
for file in "${EXISTING_FILES[@]}"; do
    chmod +x "$file"
    echo "✅ $file"
done

# 显示结果
echo ""
echo "📋 权限添加完成:"
echo "成功: ${#EXISTING_FILES[@]} 个文件"
[ ${#MISSING_FILES[@]} -gt 0 ] && echo "缺失: ${#MISSING_FILES[@]} 个文件"

echo ""
echo "当前权限:"
for file in "${EXISTING_FILES[@]}"; do
    ls -l "$file"
done