#!/bin/sh
if [ ! $# = 1 ]
then
	echo "Enter file name";
	exit;
else
	if [ -f "$1" ]
	then
		echo "File exists";
	else
echo "File doesn't exist";
	fi
fi

