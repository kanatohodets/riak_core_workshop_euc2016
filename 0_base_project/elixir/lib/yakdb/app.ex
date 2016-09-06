defmodule YakDB.App do
  use Application
  def start(_type, _args) do
    case YakDB.Sup.start_link() do
      {:ok, pid} ->
        :ok = :riak_core.register([{:vnode_module, YakDB.Vnode}])
        :ok = :riak_core_node_watcher.service_up(YakDB.Service, self())
        {:ok, pid}
      {:error, reason} ->
        {:error, reason}
    end
  end
end
