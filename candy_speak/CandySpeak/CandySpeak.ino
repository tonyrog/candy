// -*- c++ -*-
#include <stdint.h>
#include <memory.h>
#include <ctype.h>

#include "CandySpeak.h"

int fixpoint = 0;

void setup() {
  Serial.begin(9600);  
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB port only
  }
  Serial.println("CandySpeak starting");
  // put your setup code here, to run once:
  candy_init();
  // verbose = 1;  // serial print  info
}

char linebuf[MIN_LINE_LENGTH];
int  pos = 0;

void loop() {
  tick_t t;
  if (Serial.available() > 0) {
    linebuf[pos++] = Serial.read();
        if ((pos == sizeof(linebuf)-1) || (linebuf[pos-1] == '\n')) {
      linebuf[pos] = '\0';
      candy_parse_line(linebuf);
      pos = 0;
    }
  }
  t = time_tick();
  candy_read_input(t);  
  if (nevents) {
    candy_run_event(&event);
    nevents = 0;
  }
  do {
      candy_run_rules();
  } while(fixpoint && nupdates);
  
  // candy_emit_frames(t);
  candy_write_output(t);
}
