# frozen_string_literal: true  -*- mode: ruby-ts; -*-

if ENV.fetch("INSIDE_EMACS", false)
   puts "Inside Emacs we are.  Simple prompt we need."
   IRB.conf[:PROMPT_MODE] = :INF_RUBY
end
