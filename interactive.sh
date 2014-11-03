#! /usr/bin/env bash

t="1"
make || t="0"
while [ true ]; do
    while [ $t == "1" ] ; do
	clear
	./main
	sleep 0.5
	make || t="0"
    done

    while [ $t == "0" ]; do
	(make &> /dev/null) && t="1"
    done
done
