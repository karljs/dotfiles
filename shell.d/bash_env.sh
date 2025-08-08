# Environment variables, shell settings, and scripts that need
# to be sourced

export EDITOR="hx"
export VISUAL="hx"

# disable XON/XOFF so C-s can forward search
stty -ixon

if command -v zellij >/dev/null 2>&1; then
  eval "$(zellij setup --generate-completion bash)"
fi

# Haskell tooling
[ -f "/home/karl/.ghcup/env" ] && . "/home/karl/.ghcup/env" # ghcup-env

# Rust tooling
PATH="/home/karl/.cargo/bin:$PATH"

export PATH

# prompt
if command -v starship >/dev/null 2>&1; then
  eval "$(starship init bash)"
fi
