defmodule Kvapi.StoreController do
  require Logger
  use Phoenix.Controller
  use Kvapi.Web, :controller

  def store(%Plug.Conn{body_params: data}=conn, %{"key" => key, "n" => n, "w" => w}=params) do
    n = String.to_integer(n)
    w = String.to_integer(w)
    Logger.info("storing stuff: #{inspect conn}")
    result = HandoffKV.Service.store(key, data, n, w)
    Logger.info("storing stuff: stored! #{inspect result}")
    render conn, store: result
  end

  def fetch(conn, %{"key" => key, "n" => n, "r" => r}=params) do
    n = String.to_integer(n)
    r = String.to_integer(r)
    case HandoffKV.Service.fetch(key, n, r) do
      [] ->
        render conn, fetch: :not_found

      data ->
        cleaned = for {key, value} <- data, do: %{key => value}
        render conn, fetch: cleaned

    end
  end
end
