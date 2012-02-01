# The directory where all the configuration files are kept.
CONFIG_DIR=$( cd -P -- "$(dirname -- "$(command -v -- "$0")")" && pwd -P )

echo "export CONFIG_DIR=$CONFIG_DIR" > ~/.config_dir

for f in bash_profile bashrc inputrc emacs.d; do
    rm -rf ~/.$f
    ln -sf $CONFIG_DIR/$f ~/.$f
done

emacs --batch \
      --eval "(let ((default-directory  \"~/.emacs.d/\")) (normal-top-level-add-subdirs-to-load-path))" \
      --eval "(batch-byte-compile-if-not-done)" site-lisp/**/*.el elpa/**/*.el
