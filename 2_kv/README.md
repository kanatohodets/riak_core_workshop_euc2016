# State management

A stateful service needs to have state, and the simplest possible form of state
is a key-value store. Let's implement the most naive distributed hash table by
storing key-value pairs on a vnode based on the hash of the key.

## Exercise

Implement/study the service API:
    * `yak_db_service:store(Key, Data)` 
    * `yak_db_service:fetch(Key)`

Implement/study the business logic in the vnode:
    * `handle_command({store, Key, Data})`
    * `handle_command({fetch, Key})`

One way to implement this API is to have the vnode process manage an ETS table.
Or, if we wanted to be cool like Riak KV, we could use something like LevelDB,
RocksDB, or LMDB.
