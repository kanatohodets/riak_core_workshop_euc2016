defmodule HandoffKV.Vnode do
  @behaviour :riak_core_vnode
  require Logger
  require Record

  # for handoff
  Record.defrecord :riak_core_fold_req_v2, Record.extract(:riak_core_fold_req_v2, from_lib: "riak_core/include/riak_core_vnode.hrl")

  def start_vnode(partition) do
    :riak_core_vnode_master.get_vnode_pid(partition, __MODULE__)
  end

  def init([partition]) do
    ets_handle = :ets.new(nil, [])
    {:ok, %{db: ets_handle, part: partition}}
  end

  def handle_command({req_id, :ping}, _sender, %{part: partition} = state) do
    Logger.info("I am #{inspect node()} (#{inspect self()}) and I'm handling a ping request for #{partition})")
    {:reply, {req_id, {:pong, partition}}, state}
  end

  # store some data
  def handle_command({req_id, {:store, key, data}}, _sender, %{db: db, part: partition}=state) do
    Logger.info("#{partition} is storing #{inspect data} under key #{key}")
    result = :ets.insert(db, {key, data})
    {:reply, {req_id, {result}}, state}
  end

  # fetch some data
  def handle_command({req_id, {:fetch, key}}, _sender, %{db: db, part: partition}=state) do
    Logger.info("#{partition} is fetching data for #{key}")
    case :ets.lookup(db, key) do
      [] ->
        {:reply, {req_id, {:not_found}}, state}
      [{key, data}] ->
        {:reply, {req_id, {key, data}}, state}
    end
  end

  def handle_handoff_command(riak_core_fold_req_v2(foldfun: visit_fun, acc0: acc0)=_fold_req, _sender, %{db: db}=state) do
    fold_fn = fn(object, acc_in) ->
      Logger.warn("folding over thing: #{inspect object}")
      # bucket type not important for us here
      acc_out = visit_fun.({:foo_bucket, object}, object, acc_in)
      Logger.warn("what is my acc out? #{inspect acc_out}")
      acc_out
    end
    final = :ets.foldl(fold_fn, acc0, db)
    {:reply, final, state}
  end

  def handoff_starting(_target_node, state) do
    {true, state}
  end

  def handoff_cancelled(state) do
    {:ok, state}
  end

  def handoff_finished(_target_node, state) do
    {:ok, state}
  end

  def handle_handoff_data(data, %{db: db}=state) do
    {_bucket, {key, value}} = :erlang.binary_to_term(data)
    :ets.insert(db, {key, value})
    {:reply, :ok, state}
  end

  def encode_handoff_item(object_name, object_value) do
    :erlang.term_to_binary({object_name, object_value})
  end

  def is_empty(%{db: db}=state) do
    {:"$end_of_table" === :ets.first(db), state}
  end

  def delete(%{db: db, part: partition}=state) do
    Logger.warn("deleting #{partition} from #{inspect self()}")
    :ets.delete(db)
    {:ok, state}
  end

  def handle_coverage(_req, _key_spaces, _sender, state) do
    {:stop, :not_implemented, state}
  end

  def handle_exit(_pid, _reason, state) do
    {:noreply, state}
  end

  def terminate(_reason, _state) do
    :ok
  end
end
