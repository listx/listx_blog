module Unmutate
  def Unmutate.unmutate(ts)
    if ts.size < 4
      return nil
    end

    bp1_terms = every_nth0(2, ts)
    bp1 = make_BP(ts.size, false, bp1_terms)
    bp2_terms = every_nth(2, ts)
    bp2 = make_BP(ts.size, true, bp2_terms)

    if ts.uniq.size == 1
      ts
    elsif Unmutate.arithmetic?(ts)
      ts
    elsif Unmutate.mutate(bp1) == ts
      bp1
    elsif Unmutate.mutate(bp2) == ts
      bp2
    else
      nil
    end
  end

  def Unmutate.make_BP(len, make_first_term, originals)
    if originals.size < 2
      return []
    end

    o0 = originals[0]
    o1 = originals[1]
    m = (o1 - o0) / 2
    bp = []

    for n in (0..(len - 1)) do
      bp << m*n + o0
    end

    if make_first_term
      [o0 - m] + bp.take(len - 1)
    else
      bp
    end
  end

  def Unmutate.mutate(ts)
    ts.map{|t| make_odd t}
  end

  def Unmutate.arithmetic?(ts)
    if ts.size < 2
      return false
    end

    m = ts[1] - ts[0]
    ms = ts.drop(1).zip(ts.take(ts.size - 1))

    ms.each do |t1, t0|
      if (t1 - t0) != m
        return false
      end
    end

    true
  end
end

def make_odd(t)
  if t.odd? || t == 0
    t
  else
    make_odd(t/2)
  end
end

def every_nth0(n, ts)
  arr = []
  for i in (0..(ts.size - 1)) do
    if i % n == 0
      arr << ts[i]
    end
  end
  arr
end

def every_nth(n, ts)
  arr = []
  for i in (0..(ts.size - 1)) do
    if (i + 1) % n == 0
      arr << ts[i]
    end
  end
  arr
end
