#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR="vim"
alias ls='ls --color=auto'
alias l='ls -lAh'

#Aliases that govern screen orientation, rotate monitor vertically when coding.  It's way cooler... 8)
alias normal='xrandr --output HDMI3 --auto --rotate "normal" --output HDMI1 --auto --rotate "normal" --right-of HDMI3'
alias code='xrandr --output HDMI3 --auto --rotate "left" --output HDMI1 --auto --rotate "normal" --right-of HDMI3'

PS1='[\u@\h \W]\$ '
complete -cf sudo
complete -cf man

PATH="$(ruby -e 'puts Gem.user_dir')/bin: $PATH"

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
