#!/usr/bin/env python3

from lib import draw_circle_points, mirror_points_8


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


def draw_circle_midpoint_2(r):
    """ Like draw_circle_midpoint_additive, but we start drawing from (0, -r)
    and work our way up and to the right. """
    points = []
    x = 0
    y = -r
    em = 1 - r
    points.extend(mirror_points_8(x, y))
    while x < -y:
        if em < 0:
            em += (x << 1) + 3
        else:
            em += ((x + y) << 1) + 5
            # Increment em_se by 2 (total 4) if we go southeast.
            y += 1
        # Always increment em_e and em_se by 2!
        x += 1
        points.extend(mirror_points_8(x, y))
    draw_circle_points(points, r)


def draw_circle_midpoint_2p(r):
    """ Like draw_circle_midpoint_additive, but we start drawing from (0, -r)
    and work our way up and to the right. """
    points = []
    x = 0
    y = -r
    em = 1 - r
    # Initial value for (0,-r) for 2x + 3 = 0x + 3 = 3.
    em_e = 3
    # Initial value for (0,-r) for 2(x + y) + 5 = 0 - 2y + 5 = -2y + 5.
    em_ne = -(r << 1) + 5
    points.extend(mirror_points_8(x, y))
    while x < -y:
        if em < 0:
            em += em_e
        else:
            em += em_ne
            em_ne += 2
            y += 1
        em_e += 2
        em_ne += 2
        x += 1
        points.extend(mirror_points_8(x, y))
    draw_circle_points(points, r)


if __name__ == "__main__":
    # These three all draw the same circle.
    draw_circle_midpoint(17)
    draw_circle_midpoint_integer(17)
    draw_circle_midpoint_additive(17)
    draw_circle_midpoint_2(17)
    draw_circle_midpoint_2p(17)
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
    draw_circle_midpoint_additive(0)
    #   0
    #  ╭─╮
    # 0│█│0
    #  ╰─╯
    #   0

    #  Signature: 4ba6b15b13cd6adb310eeab8ee1adfd0

    draw_circle_midpoint_additive(1)
    #   101
    #  ╭───╮
    # 1│·█·│1
    # 0│█+█│0
    # 1│·█·│1
    #  ╰───╯
    #   101

    #  Signature: 625c57cb30c48aeb33b48bebea893e83

    draw_circle_midpoint_additive(2)
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

    draw_circle_midpoint_additive(3)
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
