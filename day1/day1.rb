#!/usr/bin/env ruby

data = File.read("../data/q1/test4.in").split(",").map(&:strip)


dir = 0
vals = [0,0,0,0]
data.each do |part|
    case part[0]
        when "R"
            dir = (dir+1)%4
        when "L"
            dir = (dir-1)%4
    end

    vals[dir] += part[1..-1].to_i
    puts "#{part} #{vals} #{dir}"
end

puts (vals[0]-vals[2]).abs + (vals[1]-vals[3]).abs
