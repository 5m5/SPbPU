#!/bin/sh

if [ ! $# = 3 ]
then
	echo "Missing arguments"
	exit
fi

nul=$(echo "$3 == 0.0" | bc)

if ! ( echo "$3" | grep -E -q "?[0-9]+$" ) ; then
   echo "Third argument should be the number"
   exit
fi

if ! ( echo "$1" | grep -E -q "?[0-9]+$" ) ; then
	echo "First argument should be the number"
    exit
fi


case $2 in
+) 	res=$(echo "scale=5;$1+$3" | bc)
	;;
-)	res=$(echo "scale=5;$1-$3" | bc)
	;;
x|X)res=$(echo "scale=5;$1*$3" | bc)
	;;
/)
if [ $nul -eq 1 ]; then
	echo "Devide by 0"
	exit
else
	res=$(echo "scale=5;$1/$3" | bc)
fi
;;
*)res="use right operands: + - / x"
;;
esac
echo $res
