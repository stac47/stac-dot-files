# frozen_string_literal: true

if ENV["INSIDE_EMACS"] then
   puts "Inside Emacs we are.  Simple prompt we need."

   IRB.conf[:USE_MULTILINE] = nil
   IRB.conf[:USE_SINGLELINE] = false
   IRB.conf[:PROMPT_MODE] = :INF_RUBY

   IRB.conf[:USE_READLINE] = false
   IRB.conf[:USE_COLORIZE] = true
end

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
