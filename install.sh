#!/usr/bin/env sh

# BASH stuff
ln -sf "$PWD/shell.sh" "$HOME/.shell.sh"
ln -sn "$PWD/shell.d" "$HOME/.shell.d" >/dev/null 2>&1

source_command="[ -r ~/shell.sh ] && source ~/.shell.sh"
if ! grep -q -F "${source_command}" ~/.bashrc; then
  echo ".shell.sh is not sourced in .bashrc, adding now ..."
  echo "${source_command}" >> ~/.bashrc
fi

# Helix
if ! [ -d $HOME/.config/helix ]; then
  mkdir -p $HOME/.config/helix
fi
ln -sf "$PWD/helix/config.toml" "$HOME/.config/helix/config.toml"

# Zellij
if ! [ -d $HOME/.config/zellij ]; then
  mkdir -p $HOME/.config/zellij
fi
ln -sf "$PWD/zellij/config.kdl" "$HOME/.config/zellij/config.kdl"
