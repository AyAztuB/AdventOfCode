#/bin/sh

# $1 is basically day number

cp -r dayX "day$1"
cd "day$1"
mv dayX.ml "day$1.ml"
aoc d -I
