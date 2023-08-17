# frozen_string_literal: true

# Adapted from https://gist.github.com/jimweirich/4950443
def src(object, method)
  if object.respond_to?(method)
    meth = object.method(method)
  elsif object.is_a?(Class)
    meth = object.instance_method(method)
  end
  location = meth.source_location
  system("vim +#{location[1]} #{location[0]}") if location
  location
rescue NameError => ex
  nil
end
