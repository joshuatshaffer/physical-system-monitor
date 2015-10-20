import math

_led_gamma = 2.2

_gamma_lu = []
_linear_lu = []
_hex_strings = []


def _upper_bounds():
    delta = 255.0 / 256  # size_of_boxes = size_of_real_interval / num_of_boxes
    itter = 1
    while itter <= 255:
        yield itter * delta
        itter += 1


# normal function math.pow(x / 100.0, led_gamma) * 255
for y in _upper_bounds():
    _gamma_lu.append(math.pow(y / 255, 1.0 / _led_gamma) * 100)
    _linear_lu.append(y * 100 / 255)
for i in range(0, 255):
    _hex_strings.append('{:02X}'.format(i))


def _lookup(x, lu):
    for b, s in zip(lu, _hex_strings):
        if x < b:
            return s
    return 'FF'


def format_led(x):
    return _lookup(x, _gamma_lu)


def format_lin(x):
    return _lookup(x, _linear_lu)
