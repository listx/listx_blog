#include "gcd.h"

/* Find greatest common divisor. */
int gcd(int m, int r)
{
	if (r == 0)
		return m;

	return gcd(r, m % r);
}
