#-------------------------------------------------------------------------------
# Github, why do you hate me so?
# export HOMEBREW_GITHUB_API_TOKEN=3e6bb5aa1cc7e5275231c5a4eea480ee530cc1cc

#-------------------------------------------------------------------------------
# Local things
export PATH=$PATH:$HOME/bin

# Haskell, Cabal
# export PATH=$HOME/src/ghc-7.8.2/bin:$PATH
# export PATH=$HOME/.cabal/bin:$PATH
GHC_APP="/Applications/ghc-7.8.2.app/Contents"
export PATH="${HOME}/.cabal/bin:${GHC_APP}/bin:${PATH}"
# export PATH=$HOME/Library/Haskell/bin:$PATH

export PATH=/opt/local/bin:/opt/local/sbin:$PATH

#-------------------------------------------------------------------------------
# Tweak a few variables
# export SHELL="/opt/local/bin/bash"
export EDITOR="vim"
export CLICOLOR=1
export HISTIGNORE="&;ls;[bf]g:exit"
export HISTFILESIZE=2500
PS1='\[\033[1;31m\]\w/\[\033[0m\] '

shopt -s globstar

#-------------------------------------------------------------------------------
# Aliases and trivial functions

alias cdb='. bookmark.sh'

# More up-to-date emacs from console
# alias emacs='/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs'
# alias emacs='/usr/local/bin/emacs'

# node-webkit
alias nw="/Applications/node-webkit.app/Contents/MacOS/node-webkit"

# tmux
alias tmux='tmux -2'
alias tl='tmux list-sessions'
function ta() { tmux -2 attach -t $@ ;}
function tn() { tmux -2 new-session -s $@ ;}

alias ghci="ghci -fno-ghci-sandbox"

alias serve='python -m SimpleHTTPServer'

# alias vim='/Applications/MacPorts/MacVim.app/Contents/MacOS/Vim'
# alias vim='/usr/local/Cellar/macvim/7.4-73/MacVim.app/Contents/MacOS/Vim'

alias nw='/Applications/node-webkit.app/Contents/MacOS/node-webkit'

fortune -s | cowsay


