#!/usr/bin/python
# using python version 2.7
import psutil # v3.2.2 by Giampaolo Rodola
import serial # v2.7 by Chris Liechti

import atexit, math, os, signal, sys, time

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
    print("Connected to: {}\nStreaming data...".format(port_name))
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
    print("Connected to: {}\nStreaming data...".format(port_name))
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

        
def main(testing):
    while True:
        try:
            if testing:
                test_loop()
            else:
                main_loop()
        except KeyboardInterrupt:
            # allow the user to interrupt
            print("Goodbye.")
            sys.exit(0)
        except Exception,e:
            print(e)
        time.sleep(1)
        print("Retrying...")

        
# ----- Daemon Stuff - based on "A simple unix/linux daemon in Python" by Sander Marechal -----
        
def delete_pid_file():
    os.remove(pid_file)
    

def daemonize():
    """Become a daemon."""

    # Check for a pid file to see if the daemon is already runing
    try:
        with open(pid_file, 'r') as pf:
            pid = int(pf.read().strip())
    except IOError:
        pid = None

    if pid:
        message = "{0} already exists. The daemon is already running.\n"
        sys.stderr.write(message.format(pid_file))
        sys.exit(1)

    """Do a UNIX double fork thing."""

    try:
        pid = os.fork()
        if pid > 0:
            # exit first parent
            sys.exit(0)
    except OSError as err:
        sys.stderr.write('fork #1 failed: {0}\n'.format(err))
        sys.exit(1)

    # decouple from parent environment
    os.chdir('/')
    os.setsid()
    os.umask(0)

    # do second fork
    try:
        pid = os.fork()
        if pid > 0:
            # exit from second parent
            sys.exit(0)
    except OSError as err:
        sys.stderr.write('fork #2 failed: {0}\n'.format(err))
        sys.exit(1)

    # redirect standard file descriptors
    sys.stdout.flush()
    sys.stderr.flush()
    si = open(os.devnull, 'r')
    so = open(os.devnull, 'a+')
    se = open(os.devnull, 'a+')

    os.dup2(si.fileno(), sys.stdin.fileno())
    os.dup2(so.fileno(), sys.stdout.fileno())
    os.dup2(se.fileno(), sys.stderr.fileno())

    # write pidfile
    atexit.register(delete_pid_file)

    pid = str(os.getpid())
    with open(pid_file, 'w+') as f:
        f.write(pid + '\n')

    
def stop_daemon():
    """Stop the daemon."""

    # Get the pid from the pid file
    try:
        with open(pid_file, 'r') as pf:
            pid = int(pf.read().strip())
    except IOError:
        pid = None

    if not pid:
        message = "{0} does not exist. The daemon is not running.\n"
        sys.stderr.write(message.format(pid_file))
        return  # not an error in a restart

    # Try killing the daemon process
    try:
        while 1:
            os.kill(pid, signal.SIGTERM)
            time.sleep(0.1)
    except OSError as err:
        e = str(err.args)
        if e.find("No such process") > 0:
            if os.path.exists(pid_file):
                os.remove(pid_file)
        else:
            print(str(err.args))
            sys.exit(1)     

        
if __name__ == "__main__":
    if len(sys.argv) == 2:
        if 'start' == sys.argv[1]:
            daemonize()
            main(testing=False)
        elif 'stop' == sys.argv[1]:
            stop_daemon()
        elif 'restart' == sys.argv[1]:
            stop_daemon()
            daemonize()
            main(testing=False)
        elif 'test' == sys.argv[1]:
            main(testing=True)
        else:
            print("Unknown command")
            sys.exit(2)
        sys.exit(0)
    else:
        print("usage: %s start|stop|restart|test" % sys.argv[0])
        sys.exit(2)
