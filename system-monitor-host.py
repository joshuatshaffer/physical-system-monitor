#!/usr/bin/env python

import sys
import time

import psutil
import serial

from outconv import format_led, format_lin

arduino_port_name = '/dev/tty.usbmodemfd141'
baud_rate = 9600


lastCpus = [0.0] * 8
slew = 0.2


def getInfos():
    global lastCpus
    cpus = psutil.cpu_percent(percpu=True)
    lastCpus = [a*(1-slew) + b*slew for a,b in zip(lastCpus, cpus)]
    out_info = list(lastCpus)
    out_info.insert(0, psutil.virtual_memory().percent)
    out_info.append(sum(cpus)/len(cpus))
    return out_info


def updateInfos(ser):
    infos = getInfos()
    out_put = 'x%s%s%s' % (format_lin(infos[0]), "".join([format_led(x) for x in infos[1:-1]]), format_lin(infos[-1]))
    print(out_put)  # for debugging
    ser.write(out_put)


def main():
    ser = serial.Serial(arduino_port_name, baud_rate)
    print("we have connected")
    time.sleep(2)
    while True:
        updateInfos(ser)
        time.sleep(0.05)

if __name__ == '__main__':
    sys.exit(main())
