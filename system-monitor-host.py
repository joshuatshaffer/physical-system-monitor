#!/usr/bin/env python

import sys
import time

import psutil
import serial
import math

arduino_port_name = '/dev/tty.usbmodemfd141'
baud_rate = 9600


def gen_antilog():
    lookup = []
    delta = 255 / 256.0
    base = math.pow(2, 2 / 25.0)
    for x in range(1, 257):
        lookup.append(math.log(x * delta + 1, base))
    return lookup


def gen_lin():
    lookup = []
    delta = 255 / 256.0
    for x in range(1, 257):
        lookup.append(x * delta)
    return lookup


antilog = gen_antilog()
lin = gen_lin()


def transform_antilog(x):
    for y,i in zip(antilog, range(0,256)):
        if (x<y):
            return '{0:02X}'.format(i)
    return 'FF'


def transform_lin(x):
    for y,i in zip(lin, range(0,256)):
        if (x<y):
            return '{0:02X}'.format(i)
    return 'FF'


def updateInfos(ser):
    x = psutil.virtual_memory().percent
    out_put = 'x'+transform_lin(x)
    for x in psutil.cpu_percent(percpu=True):
        out_put += transform_antilog(x)
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
