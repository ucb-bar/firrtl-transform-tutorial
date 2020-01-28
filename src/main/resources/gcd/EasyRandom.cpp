#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "svdpi.h"

extern int easy_random(void) {
    static bool initialized = false;
    if (!initialized) {
        srand(time(NULL));
        initialized = true;
    }
    return rand();
}
