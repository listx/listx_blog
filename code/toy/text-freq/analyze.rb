#!/usr/bin/env ruby

require_relative './text_freq'

fname = ARGV[0]
file = File.open(fname, 'r:utf-8')
corpus = file.read

occs_l = TextFreq.freq_l(corpus)
TextFreq.disp_freq(occs_l)

puts "-" * 80

occs_w = TextFreq.freq_w(corpus)
TextFreq.disp_freq(occs_w)

file.close
