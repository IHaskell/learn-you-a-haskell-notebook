#! /usr/bin/env bash

for n in $(ls ../notebook/*.ipynb); do
    echo $n " ..."
    stack exec anchordoc < $n > ../notebook/tmp
    mv ../notebook/tmp $n
    echo "done"
done
