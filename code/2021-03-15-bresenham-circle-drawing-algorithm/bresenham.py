#!/usr/bin/env python3

from lib import draw_circle, mirror_points_8


def get_circle_points_bresenham_float_ese(r):
    """ Draw a circle using a floating point variable, F_M. Draw by moving E or
    SE."""
    points = []
    x = 0
    y = r
    # F_M is a float.
    F_M = 5 / 4 - r
    points.extend(mirror_points_8(x, y))
    while x < y:
        if F_M < 0:
            F_M += 2.0 * x + 3.0
        else:
            F_M += 2.0 * (x - y) + 5.0
            y -= 1
        x += 1
        points.extend(mirror_points_8(x, y))
    return points


def get_circle_points_bresenham_integer_ese(r):
    """ Like draw_circle_bresenham_float_ese, but F_M is an integer variable.
    """
    points = []
    x = 0
    y = r
    # F_M is an integer!
    F_M = 1 - r
    points.extend(mirror_points_8(x, y))
    while x < y:
        if F_M < 0:
            # We can use a bit-shift safely because 2*n is the same as n << 1
            # in binary, and also because F_M is an integer.
            F_M += (x << 1) + 3
        else:
            F_M += ((x - y) << 1) + 5
            y -= 1
        x += 1
        points.extend(mirror_points_8(x, y))
    return points


def get_circle_points_bresenham_integer_ese_2order(r):
    """ Like draw_circle_bresenham_integer_ese, but use 2nd-order differences
    to remove multiplication from the inner loop. """
    points = []
    x = 0
    y = r
    F_M = 1 - r
    # Initial value for (0,r) for 2x + 3 = 0x + 3 = 3.
    d_e = 3
    # Initial value for (0,r) for 2(x - y) + 5 = 0 - 2y + 5 = -2y + 5.
    d_se = -(r << 1) + 5
    points.extend(mirror_points_8(x, y))
    while x < y:
        if F_M < 0:
            F_M += d_e
        else:
            F_M += d_se
            # Increment d_se by 2 (total 4) if we go southeast.
            d_se += 2
            y -= 1
        # Always increment d_e and d_se by 2!
        d_e += 2
        d_se += 2
        x += 1
        points.extend(mirror_points_8(x, y))
    return points


def get_circle_points_bresenham_integer_ene(r):
    """ Like draw_circle_bresenham_integer_ese, but we start drawing from (0,
    -r) and work our way up and to the right (that is, move E or NE). The
    difference is that we increment, instead of decrementing, y; this feels
    more uniform because we increment x as well. """
    points = []
    x = 0
    y = -r
    F_M = 1 - r
    points.extend(mirror_points_8(x, y))
    while x < -y:
        if F_M < 0:
            F_M += (x << 1) + 3
        else:
            F_M += ((x + y) << 1) + 5
            # Increment d_se by 2 (total 4) if we go southeast.
            y += 1
        # Always increment d_e and d_se by 2!
        x += 1
        points.extend(mirror_points_8(x, y))
    return points


def get_circle_points_bresenham_integer_ene_2order(r):
    """ Like draw_circle_bresenham_integer_ene, but start from (0, -r) and move
    E or NE. Notice how we only need the addition instruction in the while loop
    (y is incremented, not decremented). """
    points = []
    x = 0
    y = -r
    F_M = 1 - r
    # Initial value for (0,-r) for 2x + 3 = 0x + 3 = 3.
    d_e = 3
    # Initial value for (0,-r) for 2(x + y) + 5 = 0 - 2y + 5 = -2y + 5.
    d_ne = -(r << 1) + 5
    points.extend(mirror_points_8(x, y))
    while x < -y:
        if F_M < 0:
            F_M += d_e
        else:
            F_M += d_ne
            d_ne += 2
            y += 1
        d_e += 2
        d_ne += 2
        x += 1
        points.extend(mirror_points_8(x, y))
    return points


def get_circle_points_bresenham_integer_ene_2order_leq(r):
    """ Like draw_circle_bresenham_integer_ene_2order, but use 'f_m <= 0'
    instead of 'f_m < 0'.
    """
    points = []
    x = 0
    y = -r
    F_M = 1 - r
    d_e = 3
    d_ne = -(r << 1) + 5
    points.extend(mirror_points_8(x, y))
    while x < -y:
        if F_M <= 0:
            F_M += d_e
        else:
            F_M += d_ne
            d_ne += 2
            y += 1
        d_e += 2
        d_ne += 2
        x += 1
        points.extend(mirror_points_8(x, y))
    return points


if __name__ == "__main__":
    # These three all draw the same circle.
    draw_circle(get_circle_points_bresenham_float_ese, 17)
    draw_circle(get_circle_points_bresenham_integer_ese, 17)
    draw_circle(get_circle_points_bresenham_integer_ese_2order, 17)
    draw_circle(get_circle_points_bresenham_integer_ene, 17)
    draw_circle(get_circle_points_bresenham_integer_ene_2order, 17)
    #    11111111                   11111111
    #    76543210987654321012345678901234567
    #   ╭───────────────────────────────────╮
    # 17│·············█████████·············│17
    # 16│···········██·········██···········│16
    # 15│·········██·············██·········│15
    # 14│·······██·················██·······│14
    # 13│······█·····················█······│13
    # 12│·····█·······················█·····│12
    # 11│····█·························█····│11
    # 10│···█···························█···│10
    #  9│···█···························█···│9
    #  8│··█·····························█··│8
    #  7│··█·····························█··│7
    #  6│·█·······························█·│6
    #  5│·█·······························█·│5
    #  4│█·································█│4
    #  3│█·································█│3
    #  2│█·································█│2
    #  1│█·································█│1
    #  0│█················+················█│0
    #  1│█·································█│1
    #  2│█·································█│2
    #  3│█·································█│3
    #  4│█·································█│4
    #  5│·█·······························█·│5
    #  6│·█·······························█·│6
    #  7│··█·····························█··│7
    #  8│··█·····························█··│8
    #  9│···█···························█···│9
    # 10│···█···························█···│10
    # 11│····█·························█····│11
    # 12│·····█·······················█·····│12
    # 13│······█·····················█······│13
    # 14│·······██·················██·······│14
    # 15│·········██·············██·········│15
    # 16│···········██·········██···········│16
    # 17│·············█████████·············│17
    #   ╰───────────────────────────────────╯
    #    11111111987654321012345678911111111
    #    76543210                   01234567

    #   Signature: f9559d995ebc266712945e4113ee83c6

    # Some edge-cases.
    draw_circle(get_circle_points_bresenham_integer_ese_2order, 0)
    #   0
    #  ╭─╮
    # 0│█│0
    #  ╰─╯
    #   0

    #  Signature: 4ba6b15b13cd6adb310eeab8ee1adfd0

    draw_circle(get_circle_points_bresenham_integer_ese_2order, 1)
    #   101
    #  ╭───╮
    # 1│·█·│1
    # 0│█+█│0
    # 1│·█·│1
    #  ╰───╯
    #   101

    #  Signature: 625c57cb30c48aeb33b48bebea893e83

    draw_circle(get_circle_points_bresenham_integer_ese_2order, 2)
    #   21012
    #  ╭─────╮
    # 2│·███·│2
    # 1│█···█│1
    # 0│█·+·█│0
    # 1│█···█│1
    # 2│·███·│2
    #  ╰─────╯
    #   21012

    #  Signature: e6539d664b9120b376d1d4cda8574b6d

    draw_circle(get_circle_points_bresenham_integer_ese_2order, 3)
    #   3210123
    #  ╭───────╮
    # 3│··███··│3
    # 2│·█···█·│2
    # 1│█·····█│1
    # 0│█··+··█│0
    # 1│█·····█│1
    # 2│·█···█·│2
    # 3│··███··│3
    #  ╰───────╯
    #   3210123

    #  Signature: 463d7ac7210badfbb18c47a313cd16aa

    draw_circle(get_circle_points_bresenham_integer_ene_2order_leq, 0)
    #   0
    #  ╭─╮
    # 0│█│0
    #  ╰─╯
    #   0
    #
    #  Signature: 4ba6b15b13cd6adb310eeab8ee1adfd0

    draw_circle(get_circle_points_bresenham_integer_ene_2order_leq, 1)
    #   101
    #  ╭───╮
    # 1│███│1
    # 0│█+█│0
    # 1│███│1
    #  ╰───╯
    #   101
    #
    #  Signature: 879ffc6eb52acdea4996c531bb3e2663

    draw_circle(get_circle_points_bresenham_integer_ene_2order_leq, 2)
    #   21012
    #  ╭─────╮
    # 2│·███·│2
    # 1│█···█│1
    # 0│█·+·█│0
    # 1│█···█│1
    # 2│·███·│2
    #  ╰─────╯
    #   21012
    #
    #  Signature: e6539d664b9120b376d1d4cda8574b6d

    draw_circle(get_circle_points_bresenham_integer_ene_2order_leq, 3)
    #   3210123
    #  ╭───────╮
    # 3│··███··│3
    # 2│·█···█·│2
    # 1│█·····█│1
    # 0│█··+··█│0
    # 1│█·····█│1
    # 2│·█···█·│2
    # 3│··███··│3
    #  ╰───────╯
    #   3210123
    #
    #  Signature: 463d7ac7210badfbb18c47a313cd16aa
