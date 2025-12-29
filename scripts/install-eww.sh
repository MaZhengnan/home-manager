#!/bin/bash

echo "正在安装 Eww..."

# 检查是否已安装 Rust
if ! command -v cargo &> /dev/null; then
    echo "安装 Rust..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    source $HOME/.cargo/env
fi

# 安装依赖
echo "安装系统依赖..."
sudo apt update
sudo apt install -y \
    libgtk-3-dev \
    libgdk-pixbuf-2.0-dev \
    libpango1.0-dev \
    libatk1.0-dev \
    libcairo2-dev \
    libglib2.0-dev \
    libssl-dev \
    libx11-dev \
    pkg-config \
    cmake \
    git

# 克隆和编译
echo "编译 Eww..."
git clone https://github.com/elkowar/eww.git
cd eww

# 检测会话类型
if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
    cargo build --release --no-default-features --features wayland
else
    cargo build --release --no-default-features --features x11
fi

# 安装
echo "安装到 ~/.local/bin..."
mkdir -p ~/.local/bin
cp target/release/eww ~/.local/bin/
chmod +x ~/.local/bin/eww

# 配置 PATH
if ! grep -q '$HOME/.local/bin' ~/.bashrc; then
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
fi

source ~/.bashrc

echo "安装完成！"
echo "运行命令: eww --version 测试安装"
