#!/usr/bin/env python3

import sys
import naive
import bresenham


def generate_golden(r_min, r_max):
    """ Create a set of golden test data and dump it to YAML. """
    algos = [
        (naive.get_circle_points_naive_4, "naive_4"),
        (naive.get_circle_points_naive_8, "naive_8"),
        (naive.get_circle_points_naive_8_faster, "naive_8_faster"),
        (naive.get_circle_points_naive_8_faster_tweaked_radius, "naive_8_faster_tweaked_radius"),
        (bresenham.get_circle_points_bresenham_float_ese, "bresenham_float_ese"),
        (bresenham.get_circle_points_bresenham_integer_ese, "bresenham_integer_ese"),
        (bresenham.get_circle_points_bresenham_integer_ese_2order, "bresenham_integer_ese_2order"),
        (bresenham.get_circle_points_bresenham_integer_ene, "bresenham_integer_ene"),
        (bresenham.get_circle_points_bresenham_integer_ene_2order, "bresenham_integer_ene_2order"),
        (bresenham.get_circle_points_bresenham_integer_ene_2order_leq, "bresenham_integer_ene_2order_leq")]

    for f, algo in algos:
        for r in range(r_min, r_max):
            points = sorted(f(r), key=lambda p: (p[0], p[1]))
            print("- algo:", algo)
            print("  radius:", r)
            print("  points:")
            for x, y in points:
                print("  - x:", x)
                print("    y:", y)


if __name__ == "__main__":
    r_min = int(sys.argv[1])
    r_max = int(sys.argv[2])
    generate_golden(r_min, r_max)
