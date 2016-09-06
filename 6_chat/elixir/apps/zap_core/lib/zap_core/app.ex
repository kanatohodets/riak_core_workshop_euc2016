defmodule ZapCore.App do
  use Application
  def start(_type, _args) do
    case ZapCore.Sup.start_link() do
      {:ok, pid} ->
        :ok = :riak_core.register([{:vnode_module, ZapCore.Vnode}])
        :ok = :riak_core_node_watcher.service_up(ZapCore.Service, self())
        {:ok, pid}
      {:error, reason} ->
        {:error, reason}
    end
  end
end
