//
//  Process CANDY SPEAK rules and actions
//

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <memory.h>
#include <ctype.h>
#define DEBUG

#include "CandySpeak/CandySpeak.h"

int main(int argc, char** argv)
{
    char linebuf[MIN_LINE_LENGTH];
    FILE* fin = stdin;
    int i = 1;
    int fixpoint = 0;

    candy_init();

    if (argc > 1) {
	if (strcmp(argv[i], "-v") == 0) {
	    verbose = 1;
	    i++;
	}
	if (strcmp(argv[i], "-f") == 0) {
	    if ((fin = fopen(argv[i+1], "r")) == NULL) {
		fprintf(stderr, "unable to open file %s\n", argv[i+1]);
		exit(1);
	    }
	    i += 2;
	}
    }

    while(1) {
	tick_t t;
	
	if (i < argc) {
	    candy_parse_line(argv[i]);
	    i++;
	}
	else {
	    if (fgets(linebuf, sizeof(linebuf), fin))
		candy_parse_line(linebuf);
	}
	t = time_tick();
	candy_read_can_frames(t);
	candy_read_input(t);
	if (nevents) {
	    candy_run_event(&event);
	    nevents = 0;
	}
	// fixme: set time/loop limit
	do {
	    candy_run_rules();
	} while(fixpoint && nupdates);

	candy_write_can_frames(t);	
	candy_write_output(t);
    }
    exit(0);
}
