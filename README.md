      ____   _  ____    __  __  ____    ______
     |    \ | ||    \  |  |/ / |    \  |___   |
     |     \| ||     \ |     \ |     \  .-`.-`
     |__/\____||__|\__\|__|\__\|__|\__\|______|

           making sysops happier one at a time!

Is it any good?
---------------

Yes. And **very** useful!

Why do I need it?
-----------------

Let's admit it, configuring Erlang applications is a **pain**. Not only
because configuration files are [usually] [1] just a bunch of Erlang
terms,  which are hard to both read **and** modify for mere humans, but
also because, in a lot of cases, configured values are more complex
than the primitives Erlang provides. So, how can `nakaz` help?

Here're the highlights:

* [YAML] [2] human-readable configuration files;
* *extensible* typing and validation framework;
* on-the-fly config reloading
* ... and more!

[1]: http://www.erlang.org/doc/man/file.html#consult-1

Still not convinced? check out our
[**screencast**](http://tiny.cc/nakaz) to see `nakaz` in action!

Ho can I use it?
----------------

(hopefuly) the usage of `nakaz` is pretty straighforward, though you
still have to keep in mind **two** things:

* `nakaz` uses [YAML] [2] as base configuration format;
* `nakaz` requires you to structure your [YAML] [2] config in a *special*
  way, which is described bellow.

The basic configuration unit in `nakaz` is a **section**, which is
represented as a **named** [mapping] [3] on the YAML side. Each
configured application can have one or more sections, for example:

```yaml
example:
  srv_conf:
    conn_type: http

  log_conf:
    log: "priv/log.txt"
    severity: debug

```

Here, [example] [4] application defines a two sections, named
`log_conf` and `srv_conf`. So, as you might have noticed, the
expected structure is simple:

* applications are defined on the **top level** of the configuration file,
* with sections, residing on the **second level**.

> "Enough YAML, show me some Erlang code, dude?!"

### Configuration path

For flexibility reasons `nakaz` doesn't allow you to actually **read**
configuration file from the code, instead, it handles reading and
parsing internally, and all **you** have to do is pass path to the
configuration file via command line:

```bash
$ erl -nakaz path/to/config.yaml
```

**Note**: the current implementation doesn't allow using multiple
configuration files, but this might change in the future versions.

### Applications

As we've already mentioned, `nakaz` represents your application
configuration as sections; what we haven't mentioned is that **every**
section will be parsed into a **typed** Erlang record! Here's an
[example] [5]:

```erlang
-module(my_awesome_app).
-behaviour(application).
-compile({parse_transform, nakaz_pt}).

-include_lib("nakaz/include/nakaz.hrl").

-type filename() :: string().

-record(srv_conf, {conn_type :: http | ssl}).
-record(log_conf, {log :: filename(),
                   severity :: debug | info | error}).

%% Application callbacks

-export([start/2, stop/1]).

%% Application callbacks

start(_StartType, _StartArgs) ->
    case ?NAKAZ_ENSURE([#srv_conf{}, #log_conf{}]) of
        ok -> example_sup:start_link();
        {error, Msg} -> io:format(Msg)
    end.

stop(_State) ->
    ok.
```

What happens here? First thing to notice is `{parse_transform, nakaz_pt}`,
this is **required** for all the record-related magic to happen. Second,
`?NAKAZ_ENSURE` macro. As the name suggests, this macro *ensures*
that the configration file actually contains all of the sections, required
by your application. Moreover, `?NAKAZ_ENSURE` also checks that the
values in those sections **exactly** match the types you've declared in
the record specs!

If anything goes wrong, the `Msg` term will contain an understable
description of the error.

#### Why records?

Probably, the use of records in `?NAKAZ_ENSURE` call looks a little
supprising, and you might be thinking
`"wtf is wrong with those crazy russians?!"`. Here's the deal, forcing
arguments to be records we actually make sure that each of them is
a valid record and is available in the module scope (which is just what
`nakaz_pt` needs!).

[2]: http://www.yaml.org
[3]: http://en.wikipedia.org/wiki/YAML#Associative_arrays
[4]: https://github.com/Spawnfest2012/holybrolly-nakaz/blob/master/example/priv/conf.yaml
[5]: https://github.com/Spawnfest2012/holybrolly-nakaz/blob/master/example/src/example_app.erl

### Accessing config sections

Whenever you need to access a specific section from the configuration
file, simply [call] [6] `?NAKAZ_USE` passing **section name** as an
argument:

```erlang
%% IMPORTANT: without this line your module won't be notified of any
%% configuration changes!
-behaviour(nakaz_user).

init([]) ->
    SrvConf = ?NAKAZ_USE(#srv_conf{}),
    LogConf = ?NAKAZ_USE(#log_conf{}),
    {ok, #state{srv_conf=SrvConf,
                log_conf=LogConf}}.
```

Three awesome facts about `?NAKAZ_USE`:

* it only allows using *ensured* sections, any other sections simply
  don't exist;
* the returned section is guaranteed to be 100% valid, because
  `?NAKAZ_ENSURE` already did all the hard work of type checking and
  validating configuration values;
* the caller will be notified of section changes, see [nakaz_user] [7]
  documentation for details.

[6]: https://github.com/Spawnfest2012/holybrolly-nakaz/blob/master/example/src/example_srv.erl#L38
[7]: https://github.com/Spawnfest2012/holybrolly-nakaz/blob/master/src/nakaz_user.erl

Why the name?
-------------

This is the dumbest name we could come up with, which roughly
[translates] [8] to `mandate` from old-Russian.

[8]: http://translate.google.com/#ru|en|%D0%BD%D0%B0%D0%BA%D0%B0%D0%B7
