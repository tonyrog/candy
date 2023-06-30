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
    char linebuf[128];
    FILE* fin = stdin;
    int i = 1;

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

    while(i < argc) {
	candy_parse_line(argv[i]);
	i++;
    }
    while(fgets(linebuf, sizeof(linebuf), fin))
	candy_parse_line(linebuf);

    while(1) {
	candy_read_input();    // read io/can/analog etc
	if (nevents) {
	    candy_run_rule(&event);
	    nevents = 0;
	}
	candy_run_rules();     // generate new value
 	candy_write_output();
    }
    exit(0);
}
