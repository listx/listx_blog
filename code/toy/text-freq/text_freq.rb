module TextFreq
  # Given a string, count every occurrence of letters a-z (case insensitively).
  def TextFreq.freq_l(src)
    # Construct the array to hold the running totals (occurrences) of each
    # letter. There are 26 letters in the alphabet, so we can just have an array
    # of 26 integers.
    occs = Array.new(26, 0)

    # Count occurrences of each letter.
    src.each_char do |c|
      if !char_to_idx(c.downcase).nil?
        occs[char_to_idx(c.downcase)] += 1
      end
    end

    occs
  end

  # Simply check if the given character belongs to the range of lowercase ASCII
  # characters that make up the alphabet. "a" is 97, and "z" is 122; the numbers
  # for bounds-checking "c" come from these two (offset by 1 to account for the
  # exclusive comparison).
  def TextFreq.char_to_idx(c)
    96 < c.ord && c.ord < 123 ? c.ord - 97 : nil
  end

  # Given a string, count every occurrence of a particular word. We define a
  # "word" as a sequence of charactes that
  #   - does not have any punctuation characters at the beginning or end, and
  #   - does not have any numbers in it
  # . We take into account that text files from Project Gutenberg use a double
  # dash for an em dash to separate two words.
  def TextFreq.freq_w(src)
    occs = {}
    words = src.split(/\W*\s\W*/).map do |w|
        w.empty? ? nil : w.downcase
      end.compact
    words.each do |w|
      # Guard against cases like "*" for bullet points and such.
      if w =~ /\w/
        if w =~ /--/
          w.split("--").each do |y|
            count_word(occs, lstrip_punc(y))
          end
        else
          count_word(occs, lstrip_punc(w))
        end
      end
    end

    occs
  end

  # Add 1 to the hash for an existing key (word); otherwise, store a new
  # instance of that word.
  def TextFreq.count_word(hash, w)
    hash.key?(w) ? hash[w] += 1 : hash.store(w, 1)
    hash
  end

  # Remove leading punctuation.
  def TextFreq.lstrip_punc(w)
    w.match(/\w.*/)[0]
  end

  # Display the frequencies of letters and or words. For letters, we are only
  # concerned about 26 different values, so we print all of them out. However
  # for words, depending on the corpus there might be thousands, or even
  # millions, of different words; thus, we only display the top 100 most common
  # words.
  def TextFreq.disp_freq(occs)
    if occs.is_a?(Array)
      sum = occs.inject(0, :+)
      occs.zip(("a".."z").to_a).sort.reverse.each do |cnt, c|
        puts "#{c} = "\
          + "%.2f%%" % (cnt/sum.to_f * 100.0)\
          + " (#{cnt} occurrences)"
      end
    else
      sum = occs.values.inject(0, :+)
      occs.sort_by {|w, cnt| cnt}.reverse.take(100).each do |w, cnt|
        puts "#{w} = "\
          + "%.2f%%" % (cnt/sum.to_f * 100.0)\
          + " (#{cnt} occurrences)"
      end
    end
  end
end
