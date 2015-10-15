#include "Arduino.h"
#include "Hexinator.h"


/* Returns the hexidesimal value of d
 * Returns -1 if d is not a hex digit.
 */
int digitVal (int& d) {
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
    default: return -1; // for error
  }
}

/* If the next byte is a hex digit, place its value in d and return true,
 * else leave the next byte in the buffer and return false.
 */
boolean getDigit(int& d) {
  d = Serial.peek();
  d = digitVal(d);
  if ( d < 0 ) {
    return false;
  } else {
    Serial.read();
    return true;
  }
}

/* Parses the next "digits" bytes as a zero padded hexidesimal and puts that value
 * in "out." If it encounters a byte that does not make sence, it returns without 
 * removing it from the Serial
 */
boolean get_num(int& out) {
  int a = 0, b=0;
  if (getDigit(a) && getDigit(b)) {
    out = a * 16 + b;
    return true;
  } else {
    return false;
  }
}

