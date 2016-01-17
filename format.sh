#! /bin/sh

# 1. format all haskell source code using stylish-haskell.
# 2. convert end of line to LF style and remove bom with dos2unix.

for f in $(find ./ -type f -iname "*.hs"); do
    echo "formatting" $f "...";
    stylish-haskell -c ./.stylish-haskell.yaml --inplace $f;
    dos2unix --quiet --keepdate --remove-bom $f &> /dev/null;
done;
