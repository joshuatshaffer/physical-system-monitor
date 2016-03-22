#!/bin/bash

# From http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
# Gets the directory that this script is stored in.
# Warning, spaces and such are not escaped.
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR"

PLACE=~/Applications/pysical_system_monitor

rm -rf $PLACE

mkdir $PLACE
cp -r start_psm.app $PLACE/start_pysical_system_monitor.app
cp -r stop_psm.app $PLACE/stop_pysical_system_monitor.app
mkdir $PLACE/res
cp system_monitor_host.py $PLACE/res/system_monitor_host.py
cp daemon3x.py $PLACE/res/daemon3x.py