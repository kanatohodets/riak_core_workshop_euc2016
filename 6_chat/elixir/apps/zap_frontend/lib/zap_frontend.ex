defmodule ZapFrontend do
  use Application
  require Logger

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    port = Application.get_env(:zap_frontend, :port)
    # Define workers and child supervisors to be supervised
    children = [
      # Starts a worker by calling: ZapFrontend.Worker.start_link(arg1, arg2, arg3)
      worker(Task, [ZapFrontend, :accept, [port]]),
      supervisor(ZapFrontend.ClientManager, [])
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: ZapFrontend.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def accept(port) do
	{:ok, socket} = :gen_tcp.listen(port, [:binary, packet: :line, active: false, reuseaddr: true])
	Logger.info "Accepting connections on port #{port}"
	loop_acceptor(socket)
  end

  defp loop_acceptor(socket) do
	{:ok, client} = :gen_tcp.accept(socket)
	{:ok, pid} = ZapFrontend.ClientManager.handle(client)
    :ok = :gen_tcp.controlling_process(client, pid)
	loop_acceptor(socket)
  end
end
