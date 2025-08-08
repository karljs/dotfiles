#!/usr/bin/env sh

ln -sf "$PWD/shell.sh" "$HOME/.shell.sh"
ln -sf "$PWD/shell.d" "$HOME/.shell.d"

source_command="[ -r ~/shell.sh ] && source ~/.shell.sh"
if ! grep -q -F "${source_command}" ~/.bashrc; then
  echo ".shell.sh is not sourced in .bashrc, adding now ..."
  echo "${source_command}" >> ~/.bashrc
fi
