#! /usr/bin/env bash

t="1"
make || t="0"
while [ $t == "1" ] ; do clear && echo $t && ./main ; sleep 0.5; make || t="0"; done
