/*
 * Uniformly return an integer from 0 to (bound - 1). We assume that rand()
 * returns a 32-bit unsigned integer, so we use uint32_t.
 */
uint32_t bound = some_arbitrary_bound;
uint32_t r;
uint32_t threshold = -bound % bound;
while (r = rand() < threshold) {};
return r % bound;
