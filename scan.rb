class Token
  attr_reader :kind, :value

  # Kind = {
  #   eof: 'EOF',
  #   id: 'ID',
  #   int: 'INT',
  #   '+': '+',
  #   '-': '-',
  #   '*': '*',
  #   '/': '/',
  #   '**': '**',
  #   '(': '(',
  #   ')': ')',
  #   '=': '=',
  #   clear: 'CLEAR',
  #   list: 'LIST',
  #   sqrt: 'SQRT',
  #   quit: 'QUIT',
  #   exit: 'EXIT',
  #   while: 'WHILE',
  #   if: 'IF'
  # }

  Kind = {
    '+': '+',
    '-': '-',
    '*': '*',
    '/': '/',
    '**': '**',
    '(': '(',
    ')': ')',
    '=': '=',
    eof: 'EOF',
    id: 'ID',
    int: 'INT',
    float: 'FLOAT',
    clear: 'CLEAR',
    list: 'LIST',
    sqrt: 'SQRT'
  }

  def initialize(kind, value = nil)
    @kind = kind
    @value = value
  end

  def to_s
    case @kind
    when :int
      "#{Kind[:int]}: #{@value}"
    when :id
      "#{Kind[:id]}: #{@value}"
    when :float
      "#{Kind[:float]}: #{@value}"
    else
      Kind[@kind]
    end
  end

end
  
class Scanner
  def initialize
    @input = []
    @position = 0
  end

  def set_input(input)
    @input = input
    @position = 0
  end

  def next_token
    skip_whitespace
    return Token.new(:eof) if @position >= @input.length

    case @input[@position]
    when /[a-zA-Z]/
      read_identifier
    when /\d/
      read_number
    else
      read_operator
    end
  end

  private

  def skip_whitespace
    while @position < @input.length && @input[@position] =~ /\s/
      @position += 1
    end
  end

  def read_identifier
    start = @position
    @position += 1 while @position < @input.length && @input[@position] =~ /[a-zA-Z\d]/
    identifier = @input[start...@position].join('')
    if Token::Kind.key?(identifier.to_sym)
      return Token.new(identifier.to_sym, identifier)
    end
    Token.new(:id, identifier)
  end

  def read_number
      start = @position
      is_float = false
      # increment position while the character is a digit or a decimal point
      while @position < @input.length
        if @input[@position] =~ /\d/
          @position += 1
        elsif @input[@position] == '.'
          @position += 1
          is_float = true
        else
          break
        end
      end
      number = @input[start...@position].join('').to_f
      if is_float
        Token.new(:float, number)
      else
        Token.new(:int, number)
      end
  end

  def read_operator
    start = @position
    @position += 1 while @position < @input.length && @input[@position] =~ /[+\-*\/=()]/
    operator = @input[start...@position].join('')
  
    kind = operator.to_sym
    Token.new(kind, operator)
  end
end
