#ifndef Spwm_h
#define Spwm_h

#include "Arduino.h"

#define PERIOD 1020
#define THAT_CONST (PERIOD/255)

class Spwm {
public:
  Spwm(int pin);
  void do_setup();
  void do_loop(long now);
  void set(int val);
  int get();
private:
  int _pin;
  
  int _val;
};

#endif
