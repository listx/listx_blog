# Usage: ruby test-unmutate.rb

require 'minitest/autorun'
require_relative './unmutate.rb'

class TestUnmutate < Minitest::Test
  def test_empty_list_incalculable
    assert_equal nil, Unmutate.unmutate([])
  end

  def test_all_odd_no_change
    assert_equal [1, 3, 5, 7, 9],
      Unmutate.unmutate([1, 3, 5, 7, 9])
  end

  def test_m_is_0
    assert_equal [5, 5, 5, 5, 5],
      Unmutate.unmutate([5, 5, 5, 5, 5])
  end

  def test_known_case_1
    assert_equal [1, 2, 3, 4, 5],
      Unmutate.unmutate([1, 1, 3, 1, 5])
  end

  def test_known_case_2
    assert_equal [14, 47, 80, 113, 146, 179, 212],
      Unmutate.unmutate([7, 47, 5, 113, 73, 179, 53])
  end

  def test_known_case_3
    assert_equal [1498, 999, 500, 1],
      Unmutate.unmutate([749, 999, 125, 1])
  end

  def test_known_case_4
    assert_equal [-11, 0, 11, 22, 33, 44],
      Unmutate.unmutate([-11, 0, 11, 11, 33, 11])
  end
end
