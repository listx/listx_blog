def fibonacci_doubling(n):
    """ Calculate the Nth Fibonacci number using the doubling method. """
    return _fibonacci_doubling(n)[0]


def _fibonacci_doubling(n):
    """ Calculate Nth Fibonacci number using the doubling method. Return the
    tuple (F(n), F(n+1))."""
    if n == 0:
        return (0, 1)
    else:
        a, b = _fibonacci_doubling(n >> 1)
        c = a * ((b << 1) - a)
        d = a * a + b * b
        if n & 1:
            return (d, c + d)
        else:
            return (c, d)


if __name__ == "__main__":
    for n in range(20):
        print(fibonacci_doubling(n))
    # As a demonstration of this algorithm's speed, here is a large n.
    print(fibonacci_doubling(10000))
