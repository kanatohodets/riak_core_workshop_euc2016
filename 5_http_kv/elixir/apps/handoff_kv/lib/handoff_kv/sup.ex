defmodule HandoffKV.Sup do
  use Supervisor
  def start_link do
    # riak_core appends _sup to the application name.
    Supervisor.start_link(__MODULE__, [], [name: :handoff_kv_sup])
  end
  def init(_args) do
    # riak_core appends _master to 'HandoffKV.Vnode'
    children = [
      worker(:riak_core_vnode_master, [HandoffKV.Vnode], id: HandoffKV.Vnode_master_worker),
      supervisor(HandoffKV.OpFSM.Sup, [])
    ]
    supervise(children, strategy: :one_for_one, max_restarts: 5, max_seconds: 10)
  end
end
