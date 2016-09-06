defmodule ZapFrontend.ClientManager do
  use Supervisor
  require Logger

  def handle(args) do
    Supervisor.start_child(__MODULE__, [args])
  end

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    children = [
      worker(ZapFrontend.Client, [], restart: :temporary),
    ]
    supervise(children, strategy: :simple_one_for_one, max_restarts: 10, max_seconds: 10)
  end
end
