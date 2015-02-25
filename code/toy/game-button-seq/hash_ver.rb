module GameButtonSeq
  # button_seq = ["down", "forward", "punch"]
  # name = "hadoken"
  # button_seq_db = a hash of {sequence => name}, but a sequence can have
  # multiple names if that sequence already exists.
  def GameButtonSeq.register(button_seq, name, button_seq_db)
    # If the sequence already exists, simply add the new name to the old name, and
    # store it as a list of names.
    if button_seq_db.key?(button_seq)
      button_seq_db[button_seq] = [name] + button_seq_db[button_seq]
    else
      button_seq_db[button_seq] = [name]
    end

    button_seq_db
  end

  def GameButtonSeq.on_buttons(button_seq, button_seq_db)
    found = []
    # We search the entire button_seq_db hash for the full length of N buttons
    # first, then N - 1 buttons, then N - 2 buttons, until the search input
    # becomes 0. Meanwhile, we collect any and all matches that come our way.
    # The point is to search based on the *last* input button, as this is the
    # "current" button that should "finish" whatever combination/move we were
    # trying to do.
    while button_seq.size > 0
      if button_seq_db.key?(button_seq)
        found << button_seq_db[button_seq]
      end
      # Discard oldest button press, and search again. Even if we have a match,
      # it's important to search for other matches, too.
      button_seq.shift
    end

    # Clean up, so that we get ["foo", "bar"] instead of [["foo"], ["bar"]].
    found.flatten
  end

  def GameButtonSeq.add_hist(button_hist, button_name)
    button_hist + [button_name]
  end
end
