defmodule YakDB.Service do
  def ping do
    # hash the current time ("ping" is our bucket time, artifact of riak) to
    # get a "document index", or a position in our hash ring
    doc_idx = :riak_core_util.chash_key({"ping", :erlang.term_to_binary(:os.timestamp())})
    # use that hash to identify a list of vnodes to handle this request. we ask
    # for a list with just 1 vnode, because this operation doesn't need replication
    pref_list = :riak_core_apl.get_primary_apl(doc_idx, 1, YakDB.Service)
    [{index_vnode, _type}] = pref_list
    # then actually send the request to that vnode
    # (riak core appends "_master" to YakDB.Vnode for a process name, hence the weirdness)
    :riak_core_vnode_master.sync_spawn_command(index_vnode, :ping, YakDB.Vnode_master)
  end
end
