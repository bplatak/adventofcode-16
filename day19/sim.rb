#!/usr/bin/env ruby 

## A quick'n'dirty ruby solver for part 2
## Only meant to aid reasoning in finding the patterns

def opposite(ar, i)
	dst = ((ar.length-1.0)/2.0).ceil
	left, right = (i-dst) % ar.length, (i+dst) % ar.length

	ar[right]
end


def run(ar)
	l = ar.length
	while ar.length > 1 
		(1..l).each do |i|
			next unless ar.include? i
			ar -= [opposite(ar, ar.find_index(i))]
		end 
	end

	ar[0]
end