defmodule Kvapi.KvController do
  use Kvapi.Web, :controller
  def index(conn, _params) do
    render conn, "index.html"
  end
end
