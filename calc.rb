require_relative 'scan'

class Parser
  def initialize(scanner, variables = {})
    @scanner = scanner
    @variables = variables
    @current_token = nil
  end

  def parse
    advance_tokens
    while @current_token.kind != :eof
      handle_statement
    end
  end

  private

  # update the current token to be the next token
  def advance_tokens
    @current_token = @scanner.next_token
  end

  # verify the kind and advance the token
  def match(kind)
    if @current_token.kind == kind
      advance_tokens
    else
      raise "Syntax error: Expected '#{kind}', but found '#{@current_token}'"
    end
  end

  def handle_statement
    case @current_token.kind
    when :list
      handle_list_statement
    when :clear
      handle_clear_statement
    when :id
      handle_id_statement
    else
      handle_arithmetic_statement
    end
  end

  def handle_list_statement
    match(:list)
    puts ">> Identifier list:"
    @variables.each do |id, value|
      puts "     #{id}: #{value}"
    end
  end

  def handle_clear_statement
    match(:clear)
    variable = @current_token.value
    match(:id)
    @variables.delete(variable)
    puts ">> Cleared variable: #{variable}"
  end

  def handle_id_statement
    variable = @current_token.value
    match(:id)
    if @current_token.kind == :'='
      match(:'=')
      expression = calculate_expression
      @variables[variable] = expression
      puts ">> Assigned variable: #{variable}"
    end
    puts ">> #{variable}: #{@variables[variable]}"
  end

  def handle_arithmetic_statement
    expression = calculate_expression
    puts ">> #{expression}" if expression
  end

  def calculate_expression
    operand1 = calculate_term
  
    while @current_token.kind == :'+' || @current_token.kind == :'-'
      operator = @current_token.kind
      match(operator)
      operand2 = calculate_term
      operand1 = perform_arithmetic_operation(operand1, operator, operand2)
    end
    operand1
  end  

  def calculate_term
    operand1 = calculate_power
    while @current_token.kind == :'*' || @current_token.kind == :'/'
      operator = @current_token.kind
      match(operator)
      operand2 = calculate_power
      operand1 = perform_arithmetic_operation(operand1, operator, operand2)
    end
    operand1
  end

  def calculate_power
    operand1 = calculate_factor
    while @current_token.kind == :**
      operator = @current_token.kind
      match(operator)
      operand2 = calculate_factor
      operand1 = perform_arithmetic_operation(operand1, operator, operand2)
    end
    operand1
  end

  def calculate_factor
    if @current_token.kind == :'('
      match(:'(')
      result = calculate_expression
      match(:')')
      result
    elsif @current_token.kind == :int
      result = @current_token.value
      match(:int)
      result
    elsif @current_token.kind == :sqrt
      match(:sqrt)
      match(:'(')
      inner_expression = calculate_expression
      match(:')')
      Math.sqrt(inner_expression)
    elsif @current_token.kind == :id
      result = @variables[@current_token.value]
      match(:id)
      if result.nil?
        error("Undefined identifier: #{@current_token.value}")
      else
        result
      end
    else
      error("Unexpected token: #{@current_token}")
    end
  end
  

  def perform_arithmetic_operation(operand1, operator, operand2)
    case operator
    when :+
      operand1 + operand2
    when :-
      operand1 - operand2
    when :*
      operand1 * operand2
    when :/
      operand1 / operand2
    when :**
      operand1 ** operand2
    else
      raise "Syntax error: Invalid operator"
    end
  end
end

def execute_program
  @variables = {
    'PI' => Math::PI
  }
  puts '>> Welcome to the calculator'

  scanner = Scanner.new
  parser = Parser.new(scanner, @variables)

  loop do
    input = readline.chomp
    break if input.nil? || input.downcase == 'quit' || input.downcase == 'exit'
    scanner.set_input(input.chars)

    parser.parse
  end
end

# Entry point of the program
execute_program