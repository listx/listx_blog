#!/usr/bin/env python3

from lib import draw_points, draw_circle_points, mirror_points_8


def draw_circle_midpoint(r):
    """ Draw a circle using a floating point error margin, em. """
    points = []
    x = 0
    y = r
    # em is a float.
    em = 5 / 4 - r
    points.extend(mirror_points_8(x, y))
    while x < y:
        if em < 0:
            em += 2.0 * x + 3.0
        else:
            em += 2.0 * (x - y) + 5.0
            y -= 1
        x += 1
        points.extend(mirror_points_8(x, y))
    draw_circle_points(points, r)


def draw_circle_midpoint_integer(r):
    """ Like draw_circle_midpoint, but d is an integer variable. """
    points = []
    x = 0
    y = r
    # em is an integer!
    em = 1 - r
    points.extend(mirror_points_8(x, y))
    while x < y:
        if em < 0:
            # We can use a bit-shift safely because 2*n is the same as n << 1
            # in binary, and also because em is an integer.
            em += (x << 1) + 3
        else:
            em += ((x - y) << 1) + 5
            y -= 1
        x += 1
        points.extend(mirror_points_8(x, y))
    draw_circle_points(points, r)


def draw_circle_midpoint_additive(r):
    """ Like draw_circle_midpoint, but d is an integer variable. """
    points = []
    x = 0
    y = r
    em = 1 - r
    # Initial value for (0,r) for 2x + 3 = 0x + 3 = 3.
    em_e = 3
    # Initial value for (0,r) for 2(x - y) + 5 = 0 - 2y + 5 = -2y + 5.
    em_se = -(r << 1) + 5
    points.extend(mirror_points_8(x, y))
    while x < y:
        if em < 0:
            em += em_e
        else:
            em += em_se
            # Increment em_se by 2 (total 4) if we go southeast.
            em_se += 2
            y -= 1
        # Always increment em_e and em_se by 2!
        em_e += 2
        em_se += 2
        x += 1
        points.extend(mirror_points_8(x, y))
    draw_circle_points(points, r)


if __name__ == "__main__":
    # These three all draw the same circle.
    draw_circle_midpoint(17)
    draw_circle_midpoint_integer(17)
    draw_circle_midpoint_additive(17)
    # ·············█████████·············
    # ···········██·········██···········
    # ·········██·············██·········
    # ·······██·················██·······
    # ······█·····················█······
    # ·····█·······················█·····
    # ····█·························█····
    # ···█···························█···
    # ···█···························█···
    # ··█·····························█··
    # ··█·····························█··
    # ·█·······························█·
    # ·█·······························█·
    # █·································█
    # █·································█
    # █·································█
    # █·································█
    # █················+················█
    # █·································█
    # █·································█
    # █·································█
    # █·································█
    # ·█·······························█·
    # ·█·······························█·
    # ··█·····························█··
    # ··█·····························█··
    # ···█···························█···
    # ···█···························█···
    # ····█·························█····
    # ·····█·······················█·····
    # ······█·····················█······
    # ·······██·················██·······
    # ·········██·············██·········
    # ···········██·········██···········
    # ·············█████████·············

    # Some edge-cases.
    draw_circle_midpoint_additive(0)
    # █
    draw_circle_midpoint_additive(1)
    # ·█·
    # █+█
    # ·█·
    draw_circle_midpoint_additive(2)
    # ·███·
    # █···█
    # █·+·█
    # █···█
    # ·███·
    draw_circle_midpoint_additive(3)
    # ··███··
    # ·█···█·
    # █·····█
    # █··+··█
    # █·····█
    # ·█···█·
    # ··███··
