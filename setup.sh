# The directory where all the configuration files are kept.
CONFIG_DIR=$( cd -P -- "$(dirname -- "$(command -v -- "$0")")" && pwd -P )

echo "export CONFIG_DIR='$CONFIG_DIR'" > ~/.config_dir

for f in bashrc emacs.d; do
    if [ -e "$HOME/.$f" ]; then rm -rf "$HOME/.$f"; fi
    ln -s "$CONFIG_DIR/$f" "$HOME/.$f"
done

find emacs.d -name \*.elc -delete
emacs --batch \
      --eval "(let ((default-directory  \"~/.emacs.d/\")) (normal-top-level-add-subdirs-to-load-path))" \
      --eval "(batch-byte-compile-if-not-done)" `find emacs.d -name \*.el`
