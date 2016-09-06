defmodule CoordinatedKV.Vnode do
  @behaviour :riak_core_vnode
  require Logger

  def start_vnode(partition) do
    :riak_core_vnode_master.get_vnode_pid(partition, __MODULE__)
  end

  def init([partition]) do
    ets_handle = :ets.new(nil, [])
    {:ok, %{db: ets_handle, part: partition}}
  end

  def handle_command({req_id, :ping}, _sender, %{part: partition} = state) do
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

  def handle_handoff_command(_fold_req, _sender, state) do
    {:noreply, state}
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

  def handle_handoff_data(_data, state) do
    {:reply, :ok, state}
  end

  def encode_handoff_item(_object_name, _object_value) do
    ""
  end

  def is_empty(state) do
    {true, state}
  end

  def delete(state) do
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
