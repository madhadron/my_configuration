# My configuration files are not in ~, but in a git repository
# elsewhere and symlinked in. All support files remain where they are
# in the repository, and are not symlinked, so the includes must be
# able to get to the repository. My solution is to create a file
# ~/.config_dir which contains the path to the config repository. To
# get the paths, we have to source ~/.config_dir.
source ~/.config_dir



# Git integration
[[ $PS1 && -f $CONFIG_DIR/git-completion ]] && \
    source $CONFIG_DIR/git-completion
  
PS1='\h:\W$(__git_ps1 "(%s)") \u\$ '



# Host specific customizations
if [ `uname` = Darwin ]; then
    export PATH="/Applications/Emacs24.app/Contents/MacOS/bin:/usr/local/bin:/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin:$PATH:/Applications/splunk/bin"
    alias emacs="/Applications/Emacs24.app/Contents/MacOS/Emacs"
    alias emacsclient="/Applications/Emacs24.app/Contents/MacOS/bin/emacsclient"
fi


# Miscellaneous settings
export CDPATH=".:~/data/projects:~/data/writing:~/data/code"
set -o ignoreeof
shopt -s cdspell
shopt -s cmdhist
shopt -s dotglob
shopt -s extglob

if [ -f ~/.bashrc.local ]; then
   source ~/.bashrc.local
fi
