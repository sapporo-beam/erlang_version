# erlang_version

[![Build Status](https://travis-ci.org/niku/erlang_version.svg?branch=master)](https://travis-ci.org/niku/erlang_version)

Retrieve Erlang/OTP version like `18.1`.

## Requirements

- Erlang/OTP (17.0 or later)
- rebar3

## Usage

```
$ git clone https://github.com/niku/erlang_version.git
$ cd erlang_version
$ rebar3 shell
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.1  (abort with ^G)
1> erlang_version:full().
<<"18.1">>
2> erlang_version:major().
<<"18">>
3> erlang_version:minor().
<<"1">>
4> erlang_version:patch().
<<>>
```

## License

The library is available as open source under the terms of the [MIT License](http://opensource.org/licenses/MIT).
