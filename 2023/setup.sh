#/bin/sh

# $1 is basically day number

cp -r dayX "day$1"
cd "day$1"
mv dayX.c "day$1.c"
aoc d -I
