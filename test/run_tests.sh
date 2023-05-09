#/bin/bash

INPUTS=$(find test/*.prop)

for FILE in $INPUTS; do
  NAME=$(echo $FILE | cut -d '.' -f 1 | cut -d '/' -f 2)
  echo "----- Test $FILE -----"
  ./tab $FILE -o "test/$NAME.dot" -v
  dot -Tpng "test/$NAME.dot" -o "test/$NAME.png"
done
