#include <SoftPWM.h>
#include "Hexinator.h"

uint8_t cpu_pins[8] = {2,3,4,5, 6,7,8,9};

int memory_pin = 10;
int av_cpu_pin = 11;

void setup() {
  Serial.begin(9600);
  SoftPWMBegin();
  pinMode(memory_pin, OUTPUT);
  pinMode(av_cpu_pin, OUTPUT);
}


int c, this_n, av;
long now;

void loop() {
  if (Serial.available() > 20) {
    c = Serial.read();
    if (c == 'x') {
      if (get_num(this_n)) {
        analogWrite(memory_pin, this_n);
      }
      
      for (int i=0; i<8; ++i) {
        if (get_num(this_n)) {
          SoftPWMSet(cpu_pins[i], this_n);
        } else {
          break;
        }
      }
      
      if (get_num(this_n)) {
        analogWrite(av_cpu_pin, this_n);
      }
    }
  }
}
