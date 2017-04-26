def fibonacci_doubling_iter(n):
    """ Calculate Nth Fibonacci number using the doubling method, using
    iteration. """
    ns = []
    while n:
        ns.extend([n])
        n >>= 1

    a, b = 0, 1

    while ns:
        n = ns.pop()
        c = a * ((b << 1) - a)
        d = a * a + b * b
        if n & 1:
            a, b = d, c + d
        else:
            a, b = c, d

    return a
