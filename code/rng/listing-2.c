/* Listing 2 */
int n, x, rand_limit, rand_excess;
n = 3;
rand_excess = (RAND_MAX + 1) % n;
rand_limit = RAND_MAX - rand_excess;
while (x = rand() > rand_limit) {};
return x % n;
