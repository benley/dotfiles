require 'rubygems'

begin
  require 'wirble'
  Wirble.init
  Wirble.colorize
rescue LoadError => err
  warn "Couldn't load wirble: #{err}"
end

IRB.conf[:AUTO_INDENT] = true

require 'bond'
Bond.start
