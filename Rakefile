# This rakefile provides me with tools for adding and removing the torrent from
# Transmission, the bittorrent client that I use for testing my client against.
# It has a web interface on OSX and by grabbing a session id and then making
# remote procedure calls we can control Transmission. On linux there is a
# utility called transmission-remote that does this in what I would imagine is
# a much neater way.

require "rest_client"
require "json"

# FILE_NAME = "gpl.txt"
FILE_NAME = "Hack the Planet.png"


TORRENT_PATH = "data/" + FILE_NAME + ".torrent"
DOWNLOAD_DIRECTORY = "/Users/logaan/Desktop/"
DOWNLOAD_PATH = DOWNLOAD_DIRECTORY + FILE_NAME

BASE_URL   = "localhost:9091/transmission/"
RPC_URL    = BASE_URL + "rpc"
UPLOAD_URL = BASE_URL + "upload"


task :default => [:test]

task :test => [:cycle, :compile] do
  system("./test.erl")
end

task :cycle => [:remove, :add]

task :add => [:get_session] do
  sleep(1)
  add = JSON.parse(
    RestClient.post(
      UPLOAD_URL,
      "torrents[]" => File.new(TORRENT_PATH, "r"),
      "X-Transmission-Session-Id" => @session_id
    )
  )

  raise(RuntimeError, "Add failed") unless add["success"]
  puts "Torrent added"
end

task :remove => [:get_session] do
  FileUtils.rm(DOWNLOAD_PATH) if File.exists?(DOWNLOAD_PATH)

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
  torrent= torrents.find{|t| t["name"] == FILE_NAME }

  if torrent.nil?
    puts "Torrent not found"
  else
    remove = JSON.parse(
      RestClient.post(
        RPC_URL,
        {:method => "torrent-remove", :arguments => {:ids => [torrent["id"]]}}.to_json,
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
  rescue Errno::ECONNREFUSED
    puts "Transmission is not open"
    exit
  rescue => e
    @session_id = e.response.match(/X-Transmission-Session-Id: ([^<]+)/)[1]
  end
end

task :compile do
  Dir["src/*"].each do |source_file|
    system("erlc -o ebin #{source_file}")
  end
end
