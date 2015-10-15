#!/usr/bin/env python

import sys
import os
import time

import serial

arduino_port_name = '/dev/tty.usbmodemfd141'

def main():
    ser = serial.Serial(arduino_port_name, 9600)
    time.sleep(2)
    out_put = 'x{0:02X}y{1:02X}'.format(255, 255)
    ser.write(out_put)
    time.sleep(1000)

if __name__ == '__main__':
    sys.exit(main())
