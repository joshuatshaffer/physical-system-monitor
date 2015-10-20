#!/usr/bin/env python

import sys
import os
import time

import serial

from outconv import format_led, format_lin

arduino_port_name = '/dev/tty.usbmodemfd141'


def format_allsame(x):
    s = format_lin(x)
    return 'x' + s + (format_led(x) * 8) + s


full = format_allsame(100.0)
half = format_allsame(50.0)
zero = format_allsame(0.0)


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
