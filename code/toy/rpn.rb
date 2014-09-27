def evaluate(equation_str)
	terms = equation_str.split(' ')
	stack = []
	while terms.size > 0
		term = terms.shift
		case term
		when /^\d+?$/
			stack.push(term.to_i)
		when '+', '-', '*'
			if stack.size < 2
				raise "stack too small for operator application"
			else
				b = stack.pop
				a = stack.pop
				op = term.to_sym
				c = b.send(op, a)
				stack.push(c)
			end
		else
			raise "invalid input `#{term}'"
		end
	end
	stack
end
