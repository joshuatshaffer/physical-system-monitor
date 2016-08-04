#!/usr/bin/python

import psutil # v3.2.2 by Giampaolo Rodola
import serial # v2.7 by Chris Liechti

import math
import sys
import time

led_gamma = 2.2
port_name = '/dev/tty.wchusbserial1420'
baud_rate = 9600
pid_file = '/tmp/retro-cpu-monitor.pid'


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


def main_loop():
    ser = serial.Serial(port_name, baud_rate)
    print("Connected. Streaming data.")
    while True:
        cpus = psutil.cpu_percent(percpu=True)
        ram = psutil.virtual_memory().percent
        tcpu = sum(cpus) / len(cpus)
        cpus = cpus + list(reversed(cpus)) # for testing on computer with 4 logical cores in sted of 8
        out_put = 'x' + format_lin(ram) + "".join(map(format_led, cpus)) + format_lin(tcpu)
        print(out_put)  # for debugging
        ser.write(out_put)
        time.sleep(0.1)
        

def test_loop():
    ser = serial.Serial(port_name, baud_rate)
    print("Connected. Streaming data.")
    t = 1
    i = 0
    while True:
        # make triangle wave
        if t >= 1 or t <= 0:
            x = 0
        elif t > 0.5:
            x = 200.0 - t * 200.0
        else:
            x = t * 200.0

        linx = format_lin(x)
        lin0 = format_lin(0)
        ledx = format_led(x)
        led0 = format_led(0)

        if i == 0: # flash all outputs
            out_put = 'x' + linx + ledx * 8 + linx
        elif i == 1: # flash the first output (Memory)
            out_put = 'x' + linx + led0 * 8 + lin0
        elif i == 10: # flash the last output (Total CPU)
            out_put = 'x' + lin0 + led0 * 8 + linx
        else: # flash the each of the middle outputs (CPU LEDs)
            out_put = 'x' + lin0 + led0 * (i-2) + ledx + led0 * (9-i) + lin0

        # step the counters
        t -= 0.05
        if t < 0:
            t = 1
            i += 1
            if i > 10:
                i = 0
        
        print(out_put)  # for debugging
        ser.write(out_put)
        time.sleep(0.1)


if __name__ == "__main__":
    if len(sys.argv) == 2:
        if 'start' == sys.argv[1]:
            main_loop()
        elif 'test' == sys.argv[1]:
            test_loop()
        else:
            print("Unknown command")
            sys.exit(2)
        sys.exit(0)
    else:
        print("usage: %s start|test" % sys.argv[0])
        sys.exit(2)