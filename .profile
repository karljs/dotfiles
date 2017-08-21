#-------------------------------------------------------------------------------
# bash completion
if [ -f /usr/local/etc/bash_completion ]; then
  . /usr/local/etc/bash_completion
fi

#-------------------------------------------------------------------------------
# Local things
export PATH=$HOME/.local/bin:$PATH

#-------------------------------------------------------------------------------
# Tweak a few variables
export EDITOR="nvim"
export VISUAL="nvim"
export PAGER="less"
export CLICOLOR=1
export HISTIGNORE="&;ls;[bf]g:exit"
export HISTFILESIZE=2500
PS1='\[\033[0;34m\]\w/\[\033[0m\] '

#-------------------------------------------------------------------------------
# Aliases and trivial functions

alias cdb='. bookmark.sh'

# tmux
alias tmux='tmux -2'
alias tl='tmux list-sessions'
function ta() { tmux -2 attach -t $@ ;}
function tn() { tmux -2 new-session -s $@ ;}

alias serve='python3 -m http.server'

alias scan='nmap -sP 192.168.1.*'

alias weather='curl wttr.in/97330'

alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs.sh -nw'

mkcd () {
    mkdir -p "$*"
    cd "$*"
}

edit () {
    open -a TextEdit "$*"
}

#-------------------------------------------------------------------------------
# Things that shouldn't be in a public repo
source $HOME/.bash_secrets

#-------------------------------------------------------------------------------
# Party time
fortune -s | cowsay

# OPAM configuration
. /Users/karl/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
