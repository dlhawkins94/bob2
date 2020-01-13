#!/bin/sh

function print_help () {
    echo "Usage: bob-graph [options] sb-name"
    echo "Options:"
    echo "--: write SVG file to stdout"
    echo "-p: use the specificied program to view the SVG"
    echo "-h: this"
}

PROG=firefox
SB=""
ARGS="$@"

while [[ -n "$@" ]]; do
    case "$1" in
	--) PROG="" ;;
	--program) shift; PROG="$1" ;;
	-help) print_help; exit 0 ;;
	*) SB="$1" ;;
    esac
    shift
done

DEST=/tmp/bob-graph.svg
bob graph $ARGS | dot -Tsvg > "$DEST"
if [ -n "$PROG" ]; then
    $PROG "$DEST"
else
    cat "$DEST"
    rm "$DEST"
fi
