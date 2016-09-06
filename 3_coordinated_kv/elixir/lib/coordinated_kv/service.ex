defmodule CoordinatedKV.Service do
  def ping(_n) do
    # TODO
  end

  # n must always be >= w; w the number of n destinations that we need to
  # acknowledge the write before considering it successful
  def store(key, data, n, w) when n >= w do
    # TODO
  end

  # ditto r and n
  def fetch(key, n, r) when n >= r do
    # TODO
  end
end
