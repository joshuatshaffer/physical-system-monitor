import psutil
import serial

import time
import math

led_gamma = 2.2
port_name = '/dev/tty.usbmodemFA131'
baud_rate = 9600

lcpu = 0

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
    tcpu = sum(cpus)/len(cpus)
    out_put = 'x%s%s%s' % (format_lin(ram), "".join([format_led(x) for x in cpus]), format_lin(tcpu))
    print(out_put)  # for debugging
    ser.write(out_put)


def main():
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


main()
