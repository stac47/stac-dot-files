# frozen_string_literal: true  -*- mode: ruby-ts; -*-

if ENV.fetch("INSIDE_EMACS", false)
  puts "Inside Emacs we are.  Simple prompt we need."
  IRB.conf[:PROMPT_MODE] = :SIMPLE
  IRB.conf[:USE_MULTILINE] = false
  IRB.conf[:USE_SINGLELINE] = false
end
