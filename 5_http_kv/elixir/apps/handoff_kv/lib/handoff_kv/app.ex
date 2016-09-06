defmodule HandoffKV.App do
  use Application
  def start(_type, _args) do
    case HandoffKV.Sup.start_link() do
      {:ok, pid} ->
        :ok = :riak_core.register([{:vnode_module, HandoffKV.Vnode}])
        :ok = :riak_core_node_watcher.service_up(HandoffKV.Service, self())
        {:ok, pid}
      {:error, reason} ->
        {:error, reason}
    end
  end
end
