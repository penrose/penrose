#! /bin/bash
java -cp ".:alloy4.2.jar"  Evaluator $1 $2 "${@:3}"
