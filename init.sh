#!/usr/bin/env bash
# cd ~
# git clone https://github.com/vivnep/config.vn
# ./config.vn/init.sh
set -e
OS="$(uname)"

case "$OS" in
"Darwin")
    brew tap d12frosted/emacs-plus
    brew update
    brew install stow tmux fzf nvim alacritty zsh zoxide ripgrep git make bash coreutils sioyek
    brew install emacs-plus@29 --with-native-comp
    ln -s /opt/homebrew/opt/emacs-plus@29/Emacs.app /Applications
    if [ -z "$(
        ls -A "${HOME}/gdb" 2>/dev/null
    )" ]; then
        cd ~
        mkdir -p gdb
        wget https://github.com/vivnep/config.vn/releases/download/v0.1/gdb-m1.tar.gz
        tar xf gdb-m1.tar.gz --directory=gdb
        rm gdb-m1.tar.gz
    fi
    ;;
"Linux")
    if [ -f /etc/debian_version ]; then
        INSTALL_CMD="sudo apt-get install -y"
        CHECK_INSTALL_CMD="dpkg -s"
        PKG_INFO=(
            ["alacritty"]="sudo add-apt-repository ppa:aslatter/ppa -y && sudo apt-get update && sudo apt-get install -y alacritty"
            ["nvim"]="neovim"
        )
    else
        echo "Unsupported Linux distribution"
        exit 1
    fi
    ;;
*)
    echo "Unsupported operating system: $OS"
    exit 1
    ;;
esac

CONFIG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${CONFIG_DIR}"
git submodule update --init --recursive
echo "stowing configs"
stow --dotfiles .

if ! grep -q "$(which zsh)" /etc/shells; then
    echo "Adding zsh to /etc/shells..."
    sudo sh -c "echo $(which zsh) >> /etc/shells"
    echo "zsh added to /etc/shells."
else
    echo "zsh is already in /etc/shells."
fi
if [[ ! "$SHELL" =~ "zsh" ]]; then
    chsh -s "$(which zsh)"
    echo "log out and log back in to activate the new login shell"
fi

echo "config setup complete!"
