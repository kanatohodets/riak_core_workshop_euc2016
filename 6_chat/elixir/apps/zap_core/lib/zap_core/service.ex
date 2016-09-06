defmodule ZapCore.Service do
  require Logger
  def join(_room, _client) do
    Logger.warn("ZapCore.Service.join not yet implemented")
    :ok
  end

  def say(_room, _msg) do
    Logger.warn("ZapCore.Service.say not yet implemented")
    :ok
  end
end
