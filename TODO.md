* Stop using hashtables and switch to a Set for headers instead
* Make the Server and Client requests cancelable via Lwt.cancel
* Timeouts for hanging connections using Lwt_timeout
* Integrate the SSL channel to be a new Lwt_ssl.with_channel helper?
* SSL connect to a non-SSL seems to hang (ie https://google.com:80 hangs in a Want_read)

