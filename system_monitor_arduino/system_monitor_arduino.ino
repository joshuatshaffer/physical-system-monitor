#include "SoftPWM.h"
#include "Hexinator.h"

const uint8_t pins[10] = {10, 2,3,4,5, 6,7,8,9, 11};
const long sleep_timeout = 5000;

long sleep_at = 0;

void zero_out_pins() {
  for (int i=0; i<10; ++i)
    SoftPWMSet(pins[i], 0);
}

void setup() {
  Serial.begin(9600);
  SoftPWMBegin();
  zero_out_pins();
  sleep_at = millis() + sleep_timeout;
}

void loop() {
  if (Serial.available() > 20) {
    if ('x' == Serial.read()) {
      for (int i=0; i<10; ++i) {
        int num;
        if (get_num(num)) {
          SoftPWMSet(pins[i], num);
        } else {
          break;
        }
      }
      sleep_at = millis() + sleep_timeout;
    }
  } else if (sleep_at <= millis()) {
      zero_out_pins();
      sleep_at = 0x7FFFFFFF;
  }
}
