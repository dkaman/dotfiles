#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR="emacsclient -c -a emacs "
export BROWSER="uzbl-tabbed"
alias ls='ls --color=auto'
alias l='ls -lAh'

#Aliases that govern screen orientation, rotate monitor vertically when coding.  It's way cooler... 8)
alias normal='xrandr --output HDMI3 --auto --rotate "normal" --output HDMI1 --auto --rotate "normal" --right-of HDMI3'
alias code='xrandr --output HDMI3 --auto --rotate "left" --output HDMI1 --auto --rotate "normal" --right-of HDMI3'

complete -cf sudo
complete -cf man

function md () { mkdir -p "$@" && cd "$@"; }

export PATH="/opt/chef/embedded/bin:$PATH"
export PATH="/home/dallas/.cabal/bin:$PATH"
export GIT_PS1_SHOWDIRTYSTATE=1
export PS1="\[\e[00;31m\]\u\[\e[0m\]\[\e[00;37m\]@\[\e[0m\]\[\e[00;36m\]\h\[\e[0m\]\[\e[00;37m\] \[\e[0m\]\[\e[00;35m\]\@\[\e[0m\]\[\e[00;37m\] [\[\e[0m\]\[\e[00;33m\]\w\[\e[0m\]\[\e[00;37m\]]\n:> \[\e[0m\]"

alias et="emacsclient -t "                      # used to be "emacs -nw"
alias ew="emacsclient -c -a emacs "

alias webserv="ruby -rwebrick -e'WEBrick::HTTPServer.new(:Port => 3000, :DocumentRoot => Dir.pwd).start'"

PATH="$(ruby -e 'puts Gem.user_dir')/bin:$PATH"

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
