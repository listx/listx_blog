# LICENSE: PUBLIC DOMAIN

# Interact with `irb -I . -r path/to/this/file'.

# Usage: just execute the binary as-is without any arguments. To test the RNG,
# call with the argument "rng".

U32_MAX = 0xffffffff
U64_MAX = 0xffffffffffffffff
U32_MOD = U32_MAX + 1
U64_MOD = U64_MAX + 1

KEYS_TOTAL = 1000000

class PCG32
	@state
	@inc

	def initialize(state, inc)
		@state = state
		@inc = inc
	end

	def pcg32_random_r
		oldstate = @state
		@state = (((oldstate * 6364136223846793005) % U64_MOD) +
			(@inc | 1)) % U64_MOD
		xorshifted = (((oldstate >> 18) ^ oldstate) >> 27) % U32_MOD
		rot = oldstate >> 59
		(xorshifted >> rot) | ((xorshifted << ((-rot) & 31)) % U32_MOD)
	end

	def uniform32(range)
		rand_excess = ((U32_MAX % range) + 1) % range
		rand_limit = U32_MAX - rand_excess
		while ((x = self.pcg32_random_r) > rand_limit)
		end
		x % range
	end
end

def init_array(keys, has_key, rng)
	j = 0
	for i in 0..(KEYS_TOTAL - 1)
		j += rng.uniform32(2)
		keys[i] = i + j
	end

	if has_key
		keys[rng.uniform32(KEYS_TOTAL)]
	else
		keys[KEYS_TOTAL - 1] + 1
	end
end

def binary_search(keys, key, min, max)
	list_size = (max - min) + 1
	if (list_size == 0)
		return nil
	end

	mid = (list_size / 2) + min

	if (key < keys[mid])
		binary_search(keys, key, min, mid - 1)
	elsif (key > keys[mid])
		binary_search(keys, key, mid + 1, max)
	else
		mid
	end
end

# Begin program

rng = PCG32.new(0x1234567890abcdef, 0x1234567890abcdef)

# RNG self-test.
if (ARGV == ["rng"])
	puts "Running RNG self-test."
	puts rng.pcg32_random_r
	for n in 0..999999
		rng.pcg32_random_r
	end
	puts rng.pcg32_random_r
	for n in 0..99
		puts rng.uniform32((U32_MAX / 2) + (U32_MAX / 3))
	end
	for n in 0..9
		has_key = rng.uniform32(2) == 1
		keys = []
		key = init_array(keys, has_key, rng)
		puts "last number in array #{n} for key #{key}: #{keys[KEYS_TOTAL - 1]}"
	end
	puts "Done."
	puts "END RUBY VERSION"
	exit
end

keys = []

# Stress-test 'binary_search' method.
for i in 0..19
	has_key = rng.uniform32(2) == 1
	key = init_array(keys, has_key, rng)
	min = 0
	max = KEYS_TOTAL - 1
	mid = binary_search(keys, key, min, max)
	printf("%02d - ", i + 1)
	if mid.nil?
		puts "key `#{key}' not found."
	else
		puts "key `#{key}' found at keys[#{mid}]."
	end
end
puts "END RUBY VERSION"
