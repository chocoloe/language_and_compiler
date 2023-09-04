# Chloe Hu
# xinyuh22
class Document
	def initialize
		@contents = ''
		@modified = Time.now
		@history = [@contents]
	end
	
	def contents
		@contents
	end
	
	def contents=(new_content)
		@contents = new_content
		@modified = Time.now
		@history.push(@contents)
	end
	
	def modified
		@modified
	end
	
	def size
		@contents.count
	end
	
	def undo(n=1)
		if n > @history.size
			return nil
		end
		@history.pop(n)
		@contents = @history.last
	end
end
	
class Directory
	def initialize
		@contents = Hash.new
	end
	
	def store(name, child)
		@contents[name] = child
	end
	
	def get(name)
		@contents.key(name)
	end
	
	def delete(name)
		deleted_child = @contents.delete(name)
    	deleted_child.nil? ? nil : deleted_child
	end
	
	def size
    	total_size = 0
    	@contents.each_value do |child|
      		if child.is_a?(Document)
        		total_size += 1
      		elsif child.is_a?(Directory)
        		total_size += child.size
      		end
    	end
    	total_size
  	end
  	
  	def undo(n)
  		@contents.each_value do |child|
  			if child.is_a?(Document)
  				child.undo(n)
  			elsif child.is_a?(Directory)
  				child.undo(n)
  			end	
  		end	
  	end
  	
  	def get_by_path(pathname)
  		components = file_path.split("/")
    	return nil if components.empty? || components[-1].empty?
    	get_by_path_protected(components, 0)
    end
    
protected
    def get_by_path_protected(components, index)
    	name = components[index]
    	child = @contents[name]
    	
    	if child.nil?
    		nil
    	elsif index = components.length - 1
			child.is_a?(Document) ? child : nil
		elsif child.is_a?(Directory)
			child.get_by_path_protected(components, index - 1)
		else
			nil
		end
	end  	
end	 		