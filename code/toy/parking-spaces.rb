def get_parking_spaces(t)
	if t.empty?
		return "N/A"
	else
		return (t.max - t.size) + 1
	end
end
