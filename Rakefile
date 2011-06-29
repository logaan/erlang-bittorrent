# This rakefile provides me with tools for adding and removing the torrent from
# Transmission, the bittorrent client that I use for testing my client against.
# It has a web interface on OSX and by grabbing a session id and then making
# remote procedure calls we can control Transmission. On linux there is a utility
# called transmission-remote that does this in what I would imagine is a much
# neater way.

require "rest_client"
require "json"

BASE_URL   = "localhost:9091/transmission/"
RPC_URL    = BASE_URL + "rpc"
UPLOAD_URL = BASE_URL + "upload"

task :default => [:cycle]
task :cycle => [:remove, :add]

task :add => [:get_session] do
  add = JSON.parse(
    RestClient.post(
      UPLOAD_URL,
      "torrents[]" => File.new("gpl.txt.torrent", "r"),
      "X-Transmission-Session-Id" => @session_id
    )
  )

  raise(RuntimeError, "Add failed") unless add["success"]
  puts "Torrent added"
end

task :remove => [:get_session] do
  list= JSON.parse(
    RestClient.post(
      RPC_URL,
      {:method => "torrent-get", :arguments => {:fields => [:id, :name]}}.to_json,
      :content_type => :json,
      :accept => :json,
      "X-Transmission-Session-Id" => @session_id
    )
  )

  raise(RuntimeError, "List failed") unless list["result"] == "success"
  torrents = list["arguments"]["torrents"]
  gpl_torrent= torrents.find{|t| t["name"] == "gpl.txt" }

  if gpl_torrent.nil?
    puts "Torrent not found"
  else
    remove = JSON.parse(
      RestClient.post(
        RPC_URL,
        {:method => "torrent-remove", :arguments => {:ids => [gpl_torrent["id"]]}}.to_json,
        :content_type => :json,
        :accept => :json,
        "X-Transmission-Session-Id" => @session_id
      )
    )

    raise(RuntimeError, "Remove failed") unless remove["result"] == "success"
    puts "Torrent removed"
  end
end

task :get_session do
  begin
    RestClient.get(RPC_URL)
  rescue => e
    @session_id = e.response.match(/X-Transmission-Session-Id: ([^<]+)/)[1]
  end
end

