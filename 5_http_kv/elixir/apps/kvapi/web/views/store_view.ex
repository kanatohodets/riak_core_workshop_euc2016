defmodule Kvapi.StoreView do
  require Logger
  use Kvapi.Web, :view  

  def render("store.json", %{store: store_result}) do
    {:ok, store_results} = store_result
    store_list = Enum.map(store_results, fn({x}) -> x end)
    %{data: store_list}
  end

  def render("fetch.json", %{:fetch => :not_found}) do
    %{:data => :not_found}
  end

  def render("fetch.json", %{fetch: data}) do
    %{data: data}
  end
end
