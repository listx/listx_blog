#!/usr/bin/env python3

"""
This is a text-based drawing library that draws onto a text buffer and displays
it onto STDOUT. If you want to have a square 'aspect ratio', use a perfectly
square font such as Kreative Square
(https://github.com/listx/open-relay/tree/master/KreativeSquare).
"""

import hashlib


def draw(points, buffer):
    """ Draw the given list of (x,y) points on a text (UTF) "buffer".
    """

    buffer_size = len(buffer[0])
    center = buffer_size >> 1
    # Draw origin "+" sign.
    buffer[center][center] = "+"

    # The >> 1 is to divide by 2.
    offset = (buffer_size + 1) >> 1
    for x, y in points:
        # Skip undrawable points.
        if x > offset or y > offset:
            continue
        buffer[y - offset][x - offset] = "█"

    return buffer


def create_buffer(buffer_size):
    """ Create a drawable surface. It has a grid.
    """
    # Create minimal buffer of 1 pixel by 1 pixel, if necessary.
    if buffer_size < 0:
        buffer_size = 1

    # Force buffer_size to be an odd number.
    buffer_size |= 1

    buffer = []

    for _ in range(buffer_size):
        buffer.append(["·"] * buffer_size)

    return buffer


def display_buffer(buffer):
    """ Draw the given list of (x,y) points on a text (UTF) "buffer". Here
    'buffer_size' must be a positive odd integer.
    """

    buflen = len(buffer)
    center = len(buffer) >> 1

    # Create space for rulers.
    # +1 for blank space from left edge, +1 for border
    buffer = extend(buffer, " ", len(str(center)) + 1)

    # Vertical rulers
    for y_no_offset in range(buflen):
        # Skip first and last numbers on the borders.
        if y_no_offset in (0, buflen - 1):
            continue
        y = y_no_offset - center
        msg = str(abs(y))
        # Right side vertical ruler.
        draw_text(buffer, msg, center + 1, y + 1)
        # Left side vertical ruler.
        draw_text(buffer, msg, -center - len(msg), y + 1)

    # Horizontal rulers
    for x_no_offset in range(buflen):
        # Skip first and last numbers.
        if x_no_offset in (0, buflen - 1):
            continue
        x = x_no_offset - center
        msg = str(abs(x))
        # Top ruler.
        draw_text(buffer, msg, x, center + len(msg) + 1, "topdown")
        # Bottom ruler.
        draw_text(buffer, msg, x, -center, "topdown")

    for line in buffer:
        print("".join(line))

    # Print a signature for the buffer. This way it's easy to check if a
    # drawing matches another exactly.
    signature = get_buffer_signature(buffer)
    indent = " " * len(str(center))
    print(indent, "Signature:", signature)


def get_buffer_signature(buffer):
    """ Get the 'signature' of a buffer (drawing). This was we can get
    signatures of 2 different buffers and check if they are the same. """
    m = hashlib.md5()
    for line_chars in buffer:
        for char in line_chars:
            m.update(char.encode("utf-8"))
    return m.hexdigest()


def draw_text(buffer, msg, x, y, direction="horizontal"):
    """ Write a text message onto the buffer. """
    buflen = len(buffer)
    center = buflen >> 1

    for c in msg:
        y_b = y + center
        x_b = x + center
        if x_b < buflen and y_b < buflen:
            buffer[-y_b][x_b] = c
        if direction == "topdown":
            y -= 1
        elif direction == "bottomup":
            y += 1
        else:
            x += 1


def add_border(buffer):
    """ Wrap around an ASCII-art border around a buffer. """
    width = len(buffer[0])
    bordered = []
    bordered.append(["╭"] + (["─"] * width) + ["╮"])
    for line in buffer:
        bordered.append(["│"] + line + ["│"])
    bordered.append(["╰"] + (["─"] * width) + ["╯"])
    return bordered


def extend(buffer, symbol, magnitude):
    """ Increase buffer on all sides. """
    width = len(buffer[0])
    extended = []
    for _ in range(magnitude):
        extended.insert(0, [symbol] * ((2 * magnitude) + width))
    for line in buffer:
        extended.append([symbol] * magnitude + line + [symbol] * magnitude)
    for _ in range(magnitude):
        extended.append([symbol] * ((2 * magnitude) + width))

    return extended


def draw_circle(f, r):
    """ Wrapper around the more generic draw_points(), such that we don't have
    to bother writing 2*r + 1 ourselves for the buffer_size every time.

    We also extend the buffer's margins by 3 on each side to make drawings a
    bit easier to see.
    """
    points = f(r)
    buffer = create_buffer((r << 1) + 1)
    buffer = draw(points, buffer)
    buffer = add_border(buffer)
    display_buffer(buffer)


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
