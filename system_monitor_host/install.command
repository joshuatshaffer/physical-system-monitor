#!/bin/bash

# From http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
# Gets the directory that this script is stored in.
# Warning, spaces and such are not escaped.
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR"

installLocation=~/Applications/Physical\ System\ Monitor

#Uninstall existing version
rm -rf "$installLocation"

#Install new version
mkdir -p "$installLocation"/res
cp -r start_psm.app "$installLocation"/Start\ Physical\ System\ Monitor.app
cp -r stop_psm.app "$installLocation"/Stop\ Physical\ System\ Monitor.app
cp system_monitor_host.py "$installLocation"/res/system_monitor_host.py
cp daemon3x.py "$installLocation"/res/daemon3x.py