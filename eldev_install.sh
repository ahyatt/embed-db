#! /bin/sh

# This script downloads Eldev startup script as `~/.local/bin/eldev'.

set -e


DIR=~/.local/bin

mkdir -p $DIR
curl -fsSL https://raw.githubusercontent.com/emacs-eldev/eldev/master/bin/eldev > $DIR/eldev
chmod a+x $DIR/eldev

ORIGINAL_IFS=$IFS
IFS=:
IN_PATH=

for d in $PATH; do
    IFS=$ORIGINAL_IFS
    if test "$d" = "$DIR" || (test -n `which realpath` && test `realpath "$d"` = `realpath "$DIR"`); then
        IN_PATH=yes
    fi
done

IFS=$ORIGINAL_IFS

echo "Eldev startup script has been installed."

if test -z "$IN_PATH"; then
    echo "Don't forget to add \`$DIR' to \`PATH' environment variable:"
else
    echo "Directory \`$DIR' appears to be part of \`PATH'"
    echo "environment variable, but if this script is mistaken, you can always do:"
fi

echo
echo "    export PATH=\"$DIR:\$PATH\""
