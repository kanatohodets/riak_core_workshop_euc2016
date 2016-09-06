defmodule ZapCore.Service do
  def ping(n) do
    w = n
    ring_key = {"ping", :erlang.term_to_binary(:os.timestamp())}
    {:ok, req_id} = ZapCore.OpFSM.op(:ping, ring_key, n, w)
    wait_for_reqid(req_id, 5000)
  end

  def join(room, client) do
    ring_key = {"chat", room}
    {:ok, req_id } = ZapCore.OpFSM.op({:join, room, client}, ring_key, 2, 1)
    {:ok, res} = wait_for_reqid(req_id, 5000)
    :ok
  end

  def say(room, msg) do
    ring_key = {"chat", room}
    {:ok, req_id } = ZapCore.OpFSM.op({:say, room, msg}, ring_key, 1, 1)
    {:ok, res} = wait_for_reqid(req_id, 5000)
    :ok
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
