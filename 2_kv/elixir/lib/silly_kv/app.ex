defmodule SillyKV.App do
  use Application
  def start(_type, _args) do
    case SillyKV.Sup.start_link() do
      {:ok, pid} ->
        :ok = :riak_core.register([{:vnode_module, SillyKV.Vnode}])
        :ok = :riak_core_node_watcher.service_up(SillyKV.Service, self())
        {:ok, pid}
      {:error, reason} ->
        {:error, reason}
    end
  end
end
