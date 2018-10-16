#!/usr/bin/env bash
# Usage: normalize.sh orig.jpg 1000x1000 400x400 normalized.jpg
convert $1 -gravity center -background white -extent $2 -resize $3 $4
