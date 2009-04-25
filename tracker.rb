require "rubygems"
require "sinatra"
require "pp"

get "/announce" do
  puts "===== Announce ====="
  pp params
end

get "/scrape" do
  puts "===== Scrape ====="
  pp params
end

