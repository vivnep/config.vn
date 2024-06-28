#!/usr/bin/env bash
# cd ~ && git clone https://github.com/vivnep/config.vn && ./config.vn/init.sh
set -e

OS="$(uname)"

case "$OS" in
"Darwin")
    INSTALL_CMD="brew install"
    CHECK_INSTALL_CMD="brew list"
    PKG_INFO=(
        ["alacritty"]="--cask alacritty"
        ["nvim"]="neovim"
    )
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

package_installed() {
    local pkg="$1"
    local check_pkg="${PKG_INFO[$pkg]:-$pkg}"
    check_pkg="${check_pkg##* }" # Get the last word (package name)
    case "$OS" in
    "Darwin")
        $CHECK_INSTALL_CMD "$check_pkg" &>/dev/null
        ;;
    "Linux")
        $CHECK_INSTALL_CMD "$check_pkg" &>/dev/null
        ;;
    esac
}

install_pkg() {
    local pkg="$1"
    if package_installed "$pkg"; then
        echo "$pkg is already installed."
    else
        echo "$pkg is not installed. Installing..."
        if [ -n "${PKG_INFO[$pkg]}" ]; then
            eval "${PKG_INFO[$pkg]}"
        else
            eval "$INSTALL_CMD $pkg"
        fi
    fi
}

# install packages
for pkg in stow tmux fzf nvim alacritty zsh zoxide; do
    install_pkg "$pkg"
done

CONFIG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${CONFIG_DIR}"
echo "stowing configs"
stow --dotfiles .

if ! grep -q "$(which zsh)" /etc/shells; then
    echo "Adding zsh to /etc/shells..."
    sudo sh -c "echo $(which zsh) >> /etc/shells"
    echo "zsh added to /etc/shells."
else
    echo "zsh is already in /etc/shells."
fi
chsh -s "$(which zsh)"

echo "config setup complete!"
echo "log out and log back in to activate the new login shell"
