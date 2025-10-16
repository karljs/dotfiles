# Environment variables, shell settings, and scripts that need
# to be sourced

export EDITOR="hx"
export VISUAL="hx"

# disable XON/XOFF so C-s can forward search
stty -ixon

# misc local stuff
PATH="$HOME/.local/bin:$PATH"

# Haskell tooling
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env" # ghcup-env

# Rust tooling
PATH="$HOME/.cargo/bin:$PATH"

# Custom LLVM build
PATH="$HOME/.local/llvm/bin:$PATH"

export PATH

export DEBFULLNAME="Karl Smeltzer"
export DEBEMAIL="karl.smeltzer@canonical.com"
export DEBSIGN_KEYID="049FDC317ACDCBAD0CCE5FF8CA323723B6406E60"

export BAT_THEME="gruvbox-dark"

# must come after PATH is set
if command -v zellij >/dev/null 2>&1; then
        eval "$(zellij setup --generate-completion bash)"
fi

# prompt
if command -v starship >/dev/null 2>&1; then
        eval "$(starship init bash)"
fi
