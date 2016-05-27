source ~/.config_dir

# Host specific customizations
if [ `uname` = Darwin ]; then
    alias emacsclient="/Applications/Emacs24.app/Contents/MacOS/bin/emacsclient"
fi

# SignalSense configuration
if [ -d "$HOME/murmur" ]; then
	export SIGNALSENSE_HOME="$HOME/murmur/build"
	export GOPATH="$HOME/murmur/signalsd"
	export PATH="/usr/local/go/bin:$SIGNALSENSE_HOME/bin:$GOPATH/bin:/usr/local/cuda-7.5/bin:$PATH"
	export PYTHONPATH="$SIGNALSENSE_HOME/gasper:$SIGNALSENSE_HOME/pylib"
fi

set -o ignoreeof
shopt -s cdspell
shopt -s cmdhist
shopt -s dotglob
shopt -s extglob

[[ $PS1 && -f $CONFIG_DIR/git-completion ]] && \
    source $CONFIG_DIR/git-completion

function __prompt_command() {
	local EXIT="$?"
	echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"
	PS1="\n"
	if [ $EXIT != 0 ]; then
		PS1+="$EXIT O_O"
	else
		PS1+="$EXIT ^_^"
	fi
	PS1+="\n\n"

	PS1+="$(/bin/date) $(__git_ps1 '(%s)') \u@\h\w\n\$ "
}

export PROMPT_COMMAND=__prompt_command
