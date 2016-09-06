defmodule CoordinatedKV.Service do
  def ping(n) do
    w = n
    ring_key = {"ping", :erlang.term_to_binary(:os.timestamp())}
    {:ok, req_id} = CoordinatedKV.OpFSM.op(:ping, ring_key, n, w)
    wait_for_reqid(req_id, 5000)
  end

  # n must always be >= w; w the number of n destinations that we need to
  # acknowledge the write before considering it successful
  def store(key, data, n, w) when n >= w do
    ring_key = {"store", :erlang.term_to_binary(key)}
    {:ok, req_id} = CoordinatedKV.OpFSM.op({:store, key, data}, ring_key, n, w)
    wait_for_reqid(req_id, 5000)
  end

  # ditto r and n
  def fetch(key, n, r) when n >= r do
    ring_key = {"store", :erlang.term_to_binary(key)}
    {:ok, req_id} = CoordinatedKV.OpFSM.op({:fetch, key}, ring_key, n, r)
    wait_for_reqid(req_id, 5000)
  end

  defp wait_for_reqid(req_id, timeout) do
    receive do
      {^req_id, value} ->
        {:ok, value}
    after
      timeout ->
        {:error, :timeout}
    end
  end
end
