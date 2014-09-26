def get_parking_space(t)
	t_sorted = t.sort

	i = 0
	while i < t_sorted.size
		if i < t_sorted[i]
			break
		end
		i += 1
	end

	return i
end
