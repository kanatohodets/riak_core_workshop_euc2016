defmodule CoordinatedKV.App do
  use Application
  def start(_type, _args) do
    case CoordinatedKV.Sup.start_link() do
      {:ok, pid} ->
        :ok = :riak_core.register([{:vnode_module, CoordinatedKV.Vnode}])
        :ok = :riak_core_node_watcher.service_up(CoordinatedKV.Service, self())
        {:ok, pid}
      {:error, reason} ->
        {:error, reason}
    end
  end
end
