Running:

    MIX_ENV=dev_a iex --name dev_a@127.0.0.1 -S mix
    
Connecting the client:

    nc localhost 4040
    # or
    telnet localhost 4040
    
If you have a cluster, dev_a is on 4040, dev_b on 4041, and dev_c on 4042.
