add:
	curl -F "X-Transmission-Session-Id=DxqK52Fn27W79gJgJ8ni4CSPp5lHUXAI5GRkHDahPa80mg3G" -F "torrent_files[]=@gpl.txt.torrent" localhost:9091/transmission/upload
remove:
	curl -d '{"method":"torrent-remove","arguments":{"ids":[15]}}' -H "X-Transmission-Session-Id:DxqK52Fn27W79gJgJ8ni4CSPp5lHUXAI5GRkHDahPa80mg3G" localhost:9091/transmission/rpc
list:
	curl -d '{"method":"torrent-get","arguments":{"fields":["id"]}' -H "X-Transmission-Session-Id:DxqK52Fn27W79gJgJ8ni4CSPp5lHUXAI5GRkHDahPa80mg3G" localhost:9091/transmission/rpc
session:
	curl localhost:9091/transmission/rpc
