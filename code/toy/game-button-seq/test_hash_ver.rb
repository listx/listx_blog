require 'minitest/autorun'
require_relative './hash_ver.rb'

class TestPuz < Minitest::Test

  def setup
    @db = GameButtonSeq.register(["down", "forward", "punch"], "hadoken", {})
    @db = GameButtonSeq.register(["forward", "punch"], "charger", @db)
    @db = GameButtonSeq.register(["up", "punch"], "uppercut", @db)

    # Aside: We can easily limit the size of button_hist[] by forcefully always
    # saving only the last N key presses, with N determined by the longest
    # button sequence.
    @button_hist = []

    @button_hist = GameButtonSeq.add_hist(@button_hist, "down")
    @button_hist = GameButtonSeq.add_hist(@button_hist, "down")
    @button_hist = GameButtonSeq.add_hist(@button_hist, "down")
    @button_hist = GameButtonSeq.add_hist(@button_hist, "forward")
    @button_hist = GameButtonSeq.add_hist(@button_hist, "punch")
  end

  def test_button_seq_multiple_names_entered
    db2 = GameButtonSeq.register(["up", "punch"], "uppercut_2", @db)
    button_hist2 = GameButtonSeq.add_hist(@button_hist, "up")
    button_hist2 = GameButtonSeq.add_hist(button_hist2, "punch")
    assert_equal ["uppercut_2", "uppercut"],
      GameButtonSeq.on_buttons(button_hist2, db2)
  end

  def test_button_seq_no_sequence_found
    # Add an unrecognized button.
    button_hist2 = GameButtonSeq.add_hist(@button_hist, "back")
    assert_equal [],
      GameButtonSeq.on_buttons(button_hist2, @db)
  end

  def test_button_seq_one_sequence_found
    button_hist2 = GameButtonSeq.add_hist(@button_hist, "up")
    button_hist2 = GameButtonSeq.add_hist(button_hist2, "punch")
    assert_equal ["uppercut"],
      GameButtonSeq.on_buttons(button_hist2, @db)
  end

  def test_button_seq_multiple_sequences_found
    assert_equal ["hadoken", "charger"],
      GameButtonSeq.on_buttons(@button_hist, @db)
  end
end
