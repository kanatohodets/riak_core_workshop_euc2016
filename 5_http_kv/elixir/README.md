Running:

    MIX_ENV=dev_a iex --name dev_a@127.0.0.1 -S mix phoenix.server
    
This will start both the phoenix API and the riak core application in the same VM. Woo, a stateful service!
