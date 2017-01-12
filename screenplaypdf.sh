#!/bin/sh

cat "$1" | iconv -c -f utf-8 -t ISO-8859-1 | enscript -fCourier11 --margin=108:72:72:72 -B -o - | ps2pdf - "${1}.pdf" 
