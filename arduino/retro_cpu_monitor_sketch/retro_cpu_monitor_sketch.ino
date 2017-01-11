#include "SoftPWM.h"

#include <avr/wdt.h>

const int num_pins = 6;
const uint8_t pins[num_pins] = {6, 2,3,4,5, 7};
int levels[num_pins];

boolean input_error = false;

int digitVal (const int& d) {
 switch (d) {
    case '0': return 0;
    case '1': return 1;
    case '2': return 2;
    case '3': return 3;
    case '4': return 4;
    case '5': return 5;
    case '6': return 6;
    case '7': return 7;
    case '8': return 8;
    case '9': return 9;
    case 'a': case 'A': return 10;
    case 'b': case 'B': return 11;
    case 'c': case 'C': return 12;
    case 'd': case 'D': return 13;
    case 'e': case 'E': return 14;
    case 'f': case 'F': return 15;
    default: input_error = true; return 0;
  }
}

void zero_out_pins() {
  for (int i=0; i<num_pins; ++i)
    SoftPWMSet(pins[i], 0);
}

void setup() {
  wdt_enable(WDTO_1S);
  Serial.begin(9600);
  SoftPWMBegin();
  zero_out_pins();
}

void loop() {
  if (Serial.available() > num_pins * 2) {
    if ('x' == Serial.read()) {
      
      // read the update
      for (int i=0; i<num_pins; ++i) {
        int num = digitVal(Serial.read()) * 16
                + digitVal(Serial.read());
        if (input_error)
          return;
        levels[i] = num;
      }
      
      // write the new levels
      for (int i=0; i<num_pins; ++i) {
        SoftPWMSet(pins[i], levels[i]);
      }
      
      wdt_reset();
    }
  }
}
