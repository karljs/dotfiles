# Environment variables, shell settings, and scripts that need
# to be sourced

export EDITOR="hx"
export GIT_EDITOR="hx"
export VISUAL="hx"

# disable XON/XOFF so C-s can forward search
stty -ixon

# misc local stuff
PATH="$HOME/.local/bin:$PATH"

# Haskell tooling
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env" # ghcup-env

# Rust tooling
PATH="$HOME/.cargo/bin:$PATH"

# Go tooling
PATH="$HOME/go/bin:$PATH"

export PATH

export DEBFULLNAME="Karl Smeltzer"
export DEBEMAIL="karl.smeltzer@canonical.com"
export DEBSIGN_KEYID="049FDC317ACDCBAD0CCE5FF8CA323723B6406E60"

export BAT_THEME="ansi"

export COPILOT_CUSTOM_INSTRUCTIONS_DIRS="$HOME/.config"

PATH="$HOME/.opencode/bin/:$PATH"

# must come after PATH is set
if command -v eza >/dev/null 2>&1; then
        alias ls='eza'
        alias l='eza -lbF --git'
        alias ll='eza -lbGF --git'
        alias llm='eza -lbGd --git --sort=modified'
        alias la='eza -lbhHigUmuSa --time-style=long-iso --git --color-scale'
        alias lx='eza -lbhHigUmuSa@ --time-style=long-iso --git --color-scale'

        # specialty views
        alias lS='eza -1'
        alias lt='eza --tree --level=2'
        alias l.="eza -a | grep -E '^\.'"
fi


if command -v zellij >/dev/null 2>&1; then
        eval "$(zellij setup --generate-completion bash)"
fi

if command -v wezterm >/dev/null 2>&1; then
        eval "$(wezterm shell-completion --shell bash)"
fi

# prompt
if command -v starship >/dev/null 2>&1; then
        eval "$(starship init bash)"
fi
