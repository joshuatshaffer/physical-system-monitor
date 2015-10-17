#include "Spwm.h"
#include "Hexinator.h"

Spwm indicators[8] = {
/*core 0*/Spwm(2),
/*core 1*/Spwm(3),
/*core 2*/Spwm(4),
/*core 3*/Spwm(5),

/*core 4*/Spwm(6),
/*core 5*/Spwm(7),
/*core 6*/Spwm(8),
/*core 7*/Spwm(9)
};

int memory_pin = 10;
int av_cpu_pin = 11;

void setup() {
  Serial.begin(9600);
  for (int i=0; i<8; ++i) {
    indicators[i].do_setup();
  }
  pinMode(memory_pin, OUTPUT);
  pinMode(av_cpu_pin, OUTPUT);
}


int c, this_n, av;
long now;

void loop() {
  if (Serial.available() > 18) {
    c = Serial.read();
    if (c == 'x') {
      if (get_num(this_n)) {
        analogWrite(memory_pin, this_n);
      }
      
      for (int i=0; i<8; ++i) {
        if (get_num(this_n)) {
          indicators[i].set(this_n);
        } else {
          break;
        }
      }
      
      av = 0;
      for (int i=0; i<8; ++i) {
        av += indicators[i].get();
      }
      analogWrite(av_cpu_pin, av / 8);
    }
  }
  now = micros() % PERIOD;
  for (int i=0; i<8; ++i) {
    indicators[i].do_loop(now);
  }
}
