#!/usr/bin/env python

import sys
import time

import psutil
import serial

from outconv import format_led, format_lin

arduino_port_name = '/dev/tty.usbmodemfd141'
baud_rate = 9600


def updateInfos(ser):
    x = psutil.virtual_memory().percent
    out_put = 'x' + format_lin(x)
    av = 0
    for x in psutil.cpu_percent(percpu=True):
        out_put += format_led(x)
        av += x
    out_put += format_lin(av / 8.0)
    print(out_put)  # for debugging
    ser.write(out_put)


def main():
    while True:
        try:
            ser = serial.Serial(arduino_port_name, baud_rate)
            print("we have connected")
            time.sleep(2)
            while True:
                updateInfos(ser)
                time.sleep(0.1)
        except serial.SerialException:
            print("SerialException received. Retrying in 5 seconds.")
            time.sleep(5)
        except:
            print("IDK what happened. Retrying in 5 seconds.")
            time.sleep(5)


if __name__ == '__main__':
    sys.exit(main())
