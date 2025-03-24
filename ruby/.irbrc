# frozen_string_literal: true

if ENV["INSIDE_EMACS"] then
   puts "Inside Emacs we are.  Simple prompt we need."

   IRB.conf[:USE_MULTILINE] = false
   IRB.conf[:USE_SINGLELINE] = true
   IRB.conf[:PROMPT_MODE] = :INF_RUBY
   IRB.conf[:USE_READLINE] = false
   IRB.conf[:USE_COLORIZE] = true
end
