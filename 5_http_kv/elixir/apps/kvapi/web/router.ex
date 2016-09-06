defmodule Kvapi.Router do
  use Kvapi.Web, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", Kvapi do
    pipe_through :api
    put "/store/:key", StoreController, :store
    get "/store/:key", StoreController, :fetch
  end
end
