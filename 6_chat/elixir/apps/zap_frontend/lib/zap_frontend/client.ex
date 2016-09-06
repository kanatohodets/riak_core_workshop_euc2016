defmodule ZapFrontend.Client do
  use GenServer
  require Logger

  def start_link(socket) do
    GenServer.start_link(__MODULE__, [socket])
  end

  def init([socket]) do
    # get messages delivered to this process
    :inet.setopts(socket, [active: true])
    respond(socket, "connected! 'set-name <name>' and 'join <chat name>' to get started\n")
    {:ok, %{sock: socket, name: nil}}
  end

  def handle_info({:tcp, _socket, msg}, state) do
    pieces = msg 
              |> String.trim 
              |> String.split(" ")
      
    command(pieces, state)
  end

  def handle_info({:tcp_closed, _port}, state) do
     {:stop, {:shutdown, :tcp_closed}, state}
  end

  def handle_info(msg, %{sock: socket}=state) do
    respond(socket, msg)
    {:noreply, state}
  end

  def command(["ping", n], %{sock: socket}=state) do
    n = String.to_integer(n)
    ping = ZapCore.Service.ping(n)
    respond(socket, "#{inspect(ping)}\n")
    {:noreply, state}
  end

  def command(["set-name", name], %{sock: socket}=state) do
    respond(socket, "ok your name is #{name}\n")
    {:noreply, %{ state| name: name }}
  end

  def command(_command, %{sock: socket, name: nil}=state) do
    respond(socket, "must set a name before joining/chatting\n")
    {:noreply, state}
  end

  def command(["say", room | msg_pieces], %{name: name}=state) do
    msg = "#{name}: #{ Enum.join(msg_pieces, " ") }\n"
    :ok = ZapCore.Service.say(room, msg)
    {:noreply, state}
  end

  def command(["join", room], %{sock: socket}=state) do
    :ok = ZapCore.Service.join(room, self())
    respond(socket, "joined #{room}! 'say #{room} <message>' to chat\n")
    {:noreply, state}
  end

  def command([""], %{sock: socket}=state) do
    respond(socket, "")
    {:noreply, state}
  end

  def command(msg, %{sock: socket}=state) do
    respond(socket, "sorry, I don't understand '#{msg}'\n")
    {:noreply, state}
  end

  defp respond(socket, msg) do 
    :gen_tcp.send(socket, msg)
    :gen_tcp.send(socket, "> ")
  end
end
