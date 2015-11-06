import math

_gamma = 2.2


def format_led(x):
    n = int(math.pow(x / 100.0, _gamma) * 256.0)
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