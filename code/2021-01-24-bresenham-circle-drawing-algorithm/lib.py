#!/usr/bin/env python3

def draw_points(points, canvas_size):
    """ Draw the given list of (x,y) points on a text (UTF) "canvas". Here
    'canvas_size' must be a positive odd integer.
    """
    # Handle edge case where canvas_size is 1 (a canvas of 1 pixel by 1 pixel).
    if canvas_size <= 1:
        if (0, 0) in points:
            # The "█" is the "Full # block" character, or Unicode codepoint
            # U+2588.
            print("█")
        else:
            # Draw the origin with a special character.
            print("+")
        return

    # The >> 1 is to divide by 2.
    offset = (canvas_size - 1) >> 1
    for y in range(canvas_size):
        for x in range(canvas_size):
            if (x - offset) == 0 and (y - offset) == 0:
                print("+", end="")
                continue
            # Use an offset because we're iterating through only positive
            # points (0 to canvas_size for both x and y).
            if (x - offset, y - offset) in points:
                print("█", end="")
            else:
                # This is an "interpunct" character, or U+00B7.
                print("·", end="")
            if x == canvas_size - 1:
                print("")


def draw_circle_points(points, r):
    """ Wrapper around the more generic draw_points(), such that we don't have
    to bother calculating 2*r + 1 ourselves for the canvas_size every time.
    """
    draw_points(points, (r << 1) + 1)


def mirror_points_4(x, y):
    """ Return 4-way symmetry of points. """
    return [( x,  y),
            (-x,  y),
            ( x, -y),
            (-x, -y)]


def mirror_points_8(x, y):
    """ Return 8-way symmetry of points. """
    return [( x,  y),
            ( y,  x),
            (-x,  y),
            (-y,  x),
            ( x, -y),
            ( y, -x),
            (-x, -y),
            (-y, -x)]
