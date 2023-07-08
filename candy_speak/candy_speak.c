//
//  Process CANDY SPEAK rules and actions
//

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <memory.h>
#include <ctype.h>

#include "CandySpeak/CandySpeak.h"

int main(int argc, char** argv)
{
    char linebuf[MIN_LINE_LENGTH];
    FILE* fin = stdin;
    int i = 1;
    int fixpoint = 0;

    candy_init();
    
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

    while(1) {
	if (i < argc) {
	    candy_parse_line(argv[i]);
	    i++;
	}
	else {
	    if (fgets(linebuf, sizeof(linebuf), fin))
		candy_parse_line(linebuf);
	}
	candy_read_input();
	if (nevents) {
	    candy_run_event(&event);
	    nevents = 0;
	}
	// fixme: set time/loop limit
	do {
	    candy_run_rules();
	} while(fixpoint && nupdates);

	candy_emit_frames();	
	candy_write_output();
    }
    exit(0);
}
