#-------------------------------------------------------------------------------
# Github, why do you hate me so?
export HOMEBREW_GITHUB_API_TOKEN=3e6bb5aa1cc7e5275231c5a4eea480ee530cc1cc

#-------------------------------------------------------------------------------
# Local things
export PATH=$PATH:$HOME/bin

# Haskell, Cabal
# export PATH=$PATH:$HOME/src/ghc-7.8.3/bin
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/opt/X11/lib/pkgconfig

#-------------------------------------------------------------------------------
# Tweak a few variables
export EDITOR="vim"
export CLICOLOR=1
export HISTIGNORE="&;ls;[bf]g:exit"
export HISTFILESIZE=2500
PS1='\[\033[1;34m\]\w/\[\033[0m\] '

shopt -s globstar

#-------------------------------------------------------------------------------
# Aliases and trivial functions

alias cdb='. bookmark.sh'

alias nw="/Applications/node-webkit.app/Contents/MacOS/node-webkit"

# tmux
alias tmux='tmux -2'
alias tl='tmux list-sessions'
function ta() { tmux -2 attach -t $@ ;}
function tn() { tmux -2 new-session -s $@ ;}

alias serve='python -m SimpleHTTPServer'

mkcd () {
    mkdir -p "$*"
    cd "$*"
}

#-------------------------------------------------------------------------------
# Party time
fortune -s | cowsay
