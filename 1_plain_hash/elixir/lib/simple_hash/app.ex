defmodule SimpleHash.App do
  use Application
  def start(_type, _args) do
    case SimpleHash.Sup.start_link() do
      {:ok, pid} ->
        :ok = :riak_core.register([{:vnode_module, SimpleHash.Vnode}])
        :ok = :riak_core_node_watcher.service_up(SimpleHash.Service, self())
        {:ok, pid}
      {:error, reason} ->
        {:error, reason}
    end
  end
end
