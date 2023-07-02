
#include <stdint.h>
#include <memory.h>
#include <ctype.h>

#include "CandySpeak.h"

void setup() {
  Serial.begin(9600);  
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB port only
  }
  Serial.println("CandySpeak starting");
  // put your setup code here, to run once:
  candy_init();
  verbose = 1;  // serial print  info
}

char linebuf[128+1];
int  pos = 0;

void loop() {
  if (Serial.available() > 0) {
    linebuf[pos++] = Serial.read();
    if ((pos == sizeof(linebuf)) || (linebuf[pos-1] == '\n')) {
      linebuf[pos] = '\0';
      candy_parse_line(linebuf);
      pos = 0;
    }
  }
  candy_read_input();
  if (nevents) {
    candy_run_event(&event);
    nevents = 0;
  }
  candy_run_rules();
  candy_write_output();
}
