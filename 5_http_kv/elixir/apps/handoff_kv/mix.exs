defmodule HandoffKV.Mixfile do
  use Mix.Project

  def project do
    # for some reason I couldn't get erlc_options:
    # [:nowarn_deprecated_function] to stick, so hack this here
    System.put_env("ERL_COMPILER_OPTIONS", "nowarn_deprecated_function")
    [app: :handoff_kv,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [mod: {HandoffKV.App, []},
     applications: [:riak_core, :logger]]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [
      {:pbkdf2, git: "git://github.com/marianoguerra/erlang-pbkdf2-no-history", branch: "master", override: true},
      {:exometer_core, git: "git://github.com/basho/exometer_core.git", branch: "th/correct-dependencies", override: true},
      {:riak_core, git: "git://github.com/basho/riak_core", branch: "develop"}
    ]
  end
end
