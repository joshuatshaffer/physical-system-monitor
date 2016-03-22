#!/usr/bin/env python

from daemon3x import Daemon

import psutil
import serial

import math
import sys
import time

led_gamma = 2.2
port_name = '/dev/tty.usbmodemFA131'
baud_rate = 9600


def format_led(x):
    n = int(math.pow(x / 100.0, led_gamma) * 256.0)
    if n < 0:
        n = 0
    elif n > 255:
        n = 255
    return '{:02X}'.format(n)


def format_lin(x):
    n = int(x * 2.56)
    if n < 0:
        n = 0
    elif n > 255:
        n = 255
    return '{:02X}'.format(n)


def update_monitor(ser):
    cpus = psutil.cpu_percent(percpu=True)
    ram = psutil.virtual_memory().percent
    tcpu = sum(cpus) / len(cpus)
    out_put = 'x%s%s%s' % (format_lin(ram), "".join([format_led(x) for x in cpus]), format_lin(tcpu))
    print(out_put)  # for debugging
    ser.write(out_put)


class ThisDaemon(Daemon):
    def run(self):
        while True:
            try:
                ser = serial.Serial(port_name, baud_rate)
                print("Connected. Streaming data.")
                while True:
                    update_monitor(ser)
                    time.sleep(0.1)
            except BaseException as e:
                print(e)
            time.sleep(2)


if __name__ == "__main__":
    daemon = ThisDaemon('/tmp/pysical_system_monitor.pid')
    if len(sys.argv) == 2:
        if 'start' == sys.argv[1]:
            daemon.start()
        elif 'stop' == sys.argv[1]:
            daemon.stop()
        elif 'restart' == sys.argv[1]:
            daemon.restart()
        else:
            print("Unknown command")
            sys.exit(2)
        sys.exit(0)
    else:
        print("usage: %s start|stop|restart" % sys.argv[0])
        sys.exit(2)