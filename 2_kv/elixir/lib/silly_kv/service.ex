defmodule SillyKV.Service do
  def ping do
    doc_idx = :riak_core_util.chash_key({"ping", :erlang.term_to_binary(:os.timestamp())})
    pref_list = :riak_core_apl.get_primary_apl(doc_idx, 1, SillyKV.Service)
    [{index_vnode, _type}] = pref_list
    :riak_core_vnode_master.sync_spawn_command(index_vnode, :ping, SillyKV.Vnode_master)
  end

  def store(key, data) do
    # hash our key
    doc_idx = :riak_core_util.chash_key({"store", :erlang.term_to_binary(key)})
    # fetch list of vnodes that service that part of the hash ring
    pref_list = :riak_core_apl.get_primary_apl(doc_idx, 1, SillyKV.Service)
    [{index_vnode, _type}] = pref_list
    # ask them to store the data!
    :riak_core_vnode_master.sync_spawn_command(index_vnode, {:store, key, data}, SillyKV.Vnode_master)
  end

  def fetch(key) do
    # TODO
  end
end
