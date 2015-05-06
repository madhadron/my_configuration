# My configuration files are not in ~, but in a git repository
# elsewhere and symlinked in. All support files remain where they are
# in the repository, and are not symlinked, so the includes must be
# able to get to the repository. My solution is to create a file
# ~/.config_dir which contains the path to the config repository. To
# get the paths, we have to source ~/.config_dir.
source ~/.config_dir

# Host specific customizations
if [ `uname` = Darwin ]; then
    alias emacsclient="/Applications/Emacs24.app/Contents/MacOS/bin/emacsclient"
fi

set -o ignoreeof
shopt -s cdspell
shopt -s cmdhist
shopt -s dotglob
shopt -s extglob

[[ $PS1 && -f $CONFIG_DIR/git-completion ]] && \
    source $CONFIG_DIR/git-completion

export PROMPT_COMMAND=__prompt_command

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

if [ -f ~/.bashrc.local ]; then
   source ~/.bashrc.local
fi

