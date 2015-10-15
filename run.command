#!/bin/bash

#Runs systinfo_to_serial.py with on double click on this file.

# From http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
# Gets the directory that this script is stored in.
# Warning, spaces and such are not escaped.
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

python "$DIR/systinfo_to_serial.py"