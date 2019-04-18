#!/bin/bash
size=$1

if ! [ $1 ]; then
    echo "Missing argument"
    exit
fi

let "even = $1%2"

if [ $even -eq 0 ]; then
    echo "Size doesn't should be even"
    exit
fi

if [ $1 -eq 1 ]; then
    echo "Size should be greater then 2"
    exit
fi

let "margin = $size-1"

for (( i=1; i <= $size; i++  ))
do
    for (( j=1; j <= $margin; j++  ))
    do
		echo -n " "
    done
    for (( j=$i; j >= 1; j--  ))
    do
		echo -n "* "
    done
    echo " "
    let "margin = $margin-1"
done
