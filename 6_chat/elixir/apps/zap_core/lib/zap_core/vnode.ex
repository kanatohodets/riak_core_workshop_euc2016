defmodule ZapCore.Vnode do
  @behaviour :riak_core_vnode
  require Logger
  require Record

  # for handoff
  Record.defrecord :riak_core_fold_req_v2, Record.extract(:riak_core_fold_req_v2, from_lib: "riak_core/include/riak_core_vnode.hrl")

  def start_vnode(partition) do
    :riak_core_vnode_master.get_vnode_pid(partition, __MODULE__)
  end

  def init([partition]) do
    ets_handle = :ets.new(nil, [:bag])
    {:ok, %{db: ets_handle, part: partition}}
  end

  def handle_command({req_id, :ping}, _sender, %{part: partition} = state) do
    Logger.info("I am #{inspect node()} (#{inspect self()}) and I'm handling a ping request for #{partition})")
    {:reply, {req_id, {:pong, partition}}, state}
  end

  def handle_command({req_id, {:join, room, client}}, _sender, %{db: db} = state) do
    Logger.warn("join #{inspect client} to #{inspect room}")
    :ets.insert(db, {room, client})
    {:reply, {req_id, :ok}, state}
  end

  def handle_command({req_id, {:say, room, msg}}, _sender, %{db: db} = state) do
    Logger.warn("broadcast #{inspect msg} to #{inspect room}")

    :ets.match(db, {room, :"$1"}) 
      |> Enum.each fn([pid]) -> send(pid, "#{room} - #{msg}") end

    {:reply, {req_id, :ok}, state}
  end

  def handle_handoff_command(riak_core_fold_req_v2(foldfun: visit_fun, acc0: acc0)=_fold_req, _sender, %{db: db}=state) do
    fold_fn = fn(object, acc_in) ->
      # bucket type not important for us here
      acc_out = visit_fun.({:foo_bucket, object}, object, acc_in)
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
    {_bucket, {room, client}} = :erlang.binary_to_term(data)
    :ets.insert(db, {room, client})
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
