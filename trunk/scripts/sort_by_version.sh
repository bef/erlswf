#!/bin/sh

if [ "x$1" == "x" ]; then
	echo "$0 dirname"
	exit 1
fi

VERSION=`dirname $0`/version.erl
for i in `find $1 -name \*.swf`; do
	echo -n "$i: "
	V=`$VERSION "$i"`
	if [ "$?" != "0" ]; then
		echo "error"
		continue
	fi
	echo "$V"
	[ ! -d "$V" ] && mkdir "$V"
	cp "$i" "$V"
done
