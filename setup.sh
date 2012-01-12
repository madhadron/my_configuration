# The directory where all the configuration files are kept.
CONFIG_DIR=$( cd -P -- "$(dirname -- "$(command -v -- "$0")")" && pwd -P )

echo "export CONFIG_DIR=$CONFIG_DIR" > ~/.config_dir

for f in bash_profile bashrc inputrc; do
    rm ~/.$f
    ln -sf $CONFIG_DIR/$f ~/.$f
done

rm ~/.emacs.d
ln -sf $CONFIG_DIR/emacs-starter-kit ~/.emacs.d
