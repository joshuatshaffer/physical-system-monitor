#include "SoftPWM.h"
#include "Hexinator.h"

uint8_t pins[10] = {10, 2,3,4,5, 6,7,8,9, 11};

void setup() {
  Serial.begin(9600);
  SoftPWMBegin();
  for (int i=0; i<10; ++i)
    SoftPWMSet(pins[i], 0);
}

int c, this_n;

void loop() {
  if (Serial.available() > 20) {
    c = Serial.read();
    if (c == 'x') {
      for (int i=0; i<10; ++i) {
        if (get_num(this_n)) {
          SoftPWMSet(pins[i], this_n);
        } else {
          break;
        }
      }
    }
  }
}
