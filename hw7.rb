require_relative 'scan'

scanner = Scanner.new

loop do
  input = Readline.readline('> ', true)
  break if input.nil? || input.downcase == 'quit'

  scanner.set_input(input.chars)  # Set the input once

  while (token = scanner.next_token)
    puts token.to_s
    break if token.kind == :eof
  end
end

