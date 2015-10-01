#-------------------------------------------------------------------------------
# bash completion
if [ -f /usr/local/etc/bash_completion ]; then
  . /usr/local/etc/bash_completion
fi

#-------------------------------------------------------------------------------
# Github, why do you hate me so?
export HOMEBREW_GITHUB_API_TOKEN=3e6bb5aa1cc7e5275231c5a4eea480ee530cc1cc

#-------------------------------------------------------------------------------
# Local things
export PATH=$HOME/bin:$PATH

# Haskell, Cabal
# export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/opt/X11/lib/pkgconfig
# export GHC_DOT_APP="/Applications/ghc-7.8.4.app"
# if [ -d "$GHC_DOT_APP" ]; then
#   export PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
# fi

# Coq Beta
# export PATH=/Applications/CoqIDE_8.5beta2.app/Contents/Resources/bin:$PATH

#-------------------------------------------------------------------------------
# Tweak a few variables
export EDITOR="nvim"
export VISUAL="nvim"
export PAGER="vimpager"
export CLICOLOR=1
export HISTIGNORE="&;ls;[bf]g:exit"
export HISTFILESIZE=2500
PS1='\[\033[1;31m\]\w/\[\033[0m\] '

shopt -s globstar

#-------------------------------------------------------------------------------
# Aliases and trivial functions

alias cdb='. bookmark.sh'

# tmux
alias tmux='tmux -2'
alias tl='tmux list-sessions'
function ta() { tmux -2 attach -t $@ ;}
function tn() { tmux -2 new-session -s $@ ;}

alias serve='python3 -m http.server'

mkcd () {
    mkdir -p "$*"
    cd "$*"
}

alias vim=nvim

#-------------------------------------------------------------------------------
# Party time
fortune -s | cowsay
