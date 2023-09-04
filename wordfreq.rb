Filename = ARGV[0]

map = Hash.new(0)

File.open(Filename, "r") do |file|
  file.each_line do |line|
    words = line.split(" ").each do |word|
      map[word] += 1
    end
  end
end

map.sort.each do |word, freq|
  puts "#{word} #{freq}"
end