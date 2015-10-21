# erlang_version

[![Build Status](https://travis-ci.org/sapporo-beam/erlang_version.svg?branch=master)](https://travis-ci.org/sapporo-beam/erlang_version)

Retrieve Erlang/OTP version like `18.1`.

## Requirements

- Erlang/OTP 17.0 or later
- (for development) rebar3

## Usage

### As independent shell

```
$ git clone https://github.com/sapporo-beam/erlang_version.git
$ cd erlang_version
$ rebar3 shell
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.1  (abort with ^G)
1> erlang_version:full().
<<"18.1">>
2> erlang_version:to_tuple().
{<<"18">>,<<"1">>,<<>>}
3> erlang_version:major().
<<"18">>
4> erlang_version:minor().
<<"1">>
5> erlang_version:patch().
<<>>
```

### From Erlang

```
$ cat rebar.config
{erl_opts, [debug_info]}.
{deps, [
  erlang_version
]}.
$ rebar3 shell
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.1  (abort with ^G)
1> erlang_version:to_tuple().
{<<"18">>,<<"1">>,<<>>}
```

### From Elixir

```
$ cat mix.exs
(snip)
  defp deps do
    [{:erlang_version, "~> 0.2"}]
  end
end
$ mix deps.get
$ iex -S mix
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Interactive Elixir (1.1.1) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> :erlang_version.to_tuple
{"18", "1", ""}
```

## License

The library is available as open source under the terms of the [MIT License](http://opensource.org/licenses/MIT).
