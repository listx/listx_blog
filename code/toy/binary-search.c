/*
 * LICENSE: PUBLIC DOMAIN
 *
 * Compile with `gcc -o binary-search-c -Wall -Wextra -Wpedantic --std=gnu11 -O2
 * binary-search.c'. Check via valgrind with `valgrind --leak-check=full
 * --show-leak-kinds=all -v path/to/binary'.
 *
 * Usage: just execute the binary as-is without any arguments. To test the RNG,
 * call with the argument "rng".
 */

#include <stdbool.h> /* bool */
#include <stdio.h>
#include <stdint.h> /* UINT32_MAX */
#include <stdlib.h> /* malloc() */
#include <string.h> /* strcmp() */
#include <inttypes.h> /* uint32_t */

typedef uint32_t u32;
typedef uint64_t u64;

/*
 * "-1" is an invalid value to be used as an index for an array (the index
 * number is what binary_search() looks for.)
 */
enum {KEY_NOT_FOUND = -1};
const int KEYS_TOTAL = 1000000;

// *Really* minimal PCG32 code / (c) 2014 M.E. O'Neill / pcg-random.org
// Licensed under Apache License 2.0 (NO WARRANTY, etc. see website)

typedef struct { u64 state;  uint64_t inc; } pcg32_random_t;

u32 pcg32_random_r(pcg32_random_t *rng)
{
	u64 oldstate = rng->state;
	rng->state = oldstate * 6364136223846793005ULL + (rng->inc|1);
	u32 xorshifted = ((oldstate >> 18u) ^ oldstate) >> 27u;
	u32 rot = oldstate >> 59u;
	return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}

u32 uniform32(int range, pcg32_random_t *rng)
{
	unsigned rand_limit, rand_excess;
	u32 x;
	rand_excess = ((UINT32_MAX % range) + 1) % range;
	rand_limit = UINT32_MAX - rand_excess;
	while ((x = pcg32_random_r(rng)) > rand_limit) {};
	return x % range;
}

/* Populate an array with increasing numbers; we randomly choose whether to skip
 * to the next number by an increment of 1 or 2, so as to initialize the array
 * slightly differently each time this function is called.
 */
u32 init_array(u32 *keys, bool has_key, pcg32_random_t *rng)
{
	int i, j;
	for (i = 0, j = 0; i < KEYS_TOTAL; i++) {
		j += uniform32(2, rng);
		keys[i] = i + j;
	}

	/*
	 * If we want to choose a key, randomly choose one from one of the
	 * existing elements; otherwise, return an impossible key (where
	 * "impossible" means a key whose value lies outside the range of values
	 * that exist in the array).
	 */
	if (has_key)
		return keys[uniform32(KEYS_TOTAL, rng)];
	else
		/* Impossible key = biggest key + 1 */
		return keys[KEYS_TOTAL - 1] + 1;
}

int binary_search(u32 *keys, u32 key, int min, int max)
{
	int list_size;
	int mid;

	list_size = (max - min) + 1;
	if (list_size == 0)
		return KEY_NOT_FOUND;

	mid = (list_size / 2) + min;

	if (key < keys[mid]) {
		return binary_search(keys, key, min, mid - 1);
	} else if (key > keys[mid]) {
		return binary_search(keys, key, mid + 1, max);
	} else {
		return mid;
	}
}

int main(int argc, char **argv)
{
	int i, min, max;
	int mid;
	bool has_key;
	u32 *keys;
	u32 key;
	pcg32_random_t rng = {0x1234567890abcdefULL, 0x1234567890abcdefULL};

	/* RNG self-test. */
	if (argc > 1 && strcmp(argv[1], "rng") == 0) {
		printf("Running RNG self-test.\n");
		printf("%"PRIu32"\n", pcg32_random_r(&rng));
		for (i = 0; i < 1000000; i++) {
			pcg32_random_r(&rng);
		}
		printf("%"PRIu32"\n", pcg32_random_r(&rng));
		for (i = 0; i < 100; i++) {
			printf("%"PRIu32"\n",
				uniform32((UINT32_MAX / 2) + (UINT32_MAX / 3),
				&rng));
		}
		keys = malloc(KEYS_TOTAL * sizeof(u32));
		for (i = 0; i < 10; i++) {
			has_key = (bool)uniform32(2, &rng);
			key = init_array(keys, has_key, &rng);
			printf("last number in array %d for key %"PRIu32": ",
				i, key);
			printf("%"PRIu32"\n", keys[KEYS_TOTAL - 1]);
		}
		printf("Done.\n");
		printf("END C VERSION\n");
		free(keys);
		return 0;
	}

	/*
	 * Allocate space for our big array of keys, as well as our
	 * in-place-modified "mid" value.
	 */
	keys = malloc(KEYS_TOTAL * sizeof(u32));

	/* Stress-test binary_search(). */
	for (i = 0; i < 20; i++) {
		has_key = (bool)uniform32(2, &rng);
		key = init_array(keys, has_key, &rng);
		min = 0;
		max = KEYS_TOTAL - 1;
		mid = binary_search(keys, key, min, max);
		printf("%02d - ", i + 1);
		if (mid == KEY_NOT_FOUND) {
			printf("key `%"PRIu32"' not found.\n", key);
		} else {
			printf("key `%"PRIu32"' found at keys[%d].\n",
				key, mid);
		}
	}

	printf("END C VERSION\n");
	free(keys);
	return 0;
}
