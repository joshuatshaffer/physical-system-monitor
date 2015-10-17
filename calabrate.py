#!/usr/bin/env python

import sys
import os
import time

import serial

arduino_port_name = '/dev/tty.usbmodemfd141'

full = 'xFFFFFFFFFFFFFFFFFFFF'
half = 'x80161616161616161680'
zero = 'x00000000000000000000'


def main():
    """:rtype : void"""
    ser = serial.Serial(arduino_port_name, 9600)
    time.sleep(2)
    while True:
        ser.write(full)
        time.sleep(5)
        ser.write(half)
        time.sleep(5)
        ser.write(zero)
        time.sleep(5)


if __name__ == '__main__':
    sys.exit(main())
