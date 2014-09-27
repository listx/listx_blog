/* Listing 3 */
int n, x, rand_limit, rand_excess;
n = 3;
rand_excess = (RAND_MAX % n) + 1; /* only difference vs listing-2.c */
rand_limit = RAND_MAX - rand_excess;
while (x = rand() > rand_limit) {};
return x % n;
