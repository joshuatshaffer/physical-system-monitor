#include "Arduino.h"
#include "Spwm.h"

Spwm::Spwm(int pin) {
  _pin = pin;
}

void Spwm::do_setup() {
  pinMode(_pin, OUTPUT);
}

void Spwm::set(int val) {
  _val = val;
}

int Spwm::get() {
  return _val;
}

void Spwm::do_loop(long now) {
  digitalWrite(_pin, (now < _val * THAT_CONST) ? HIGH : LOW);
}
