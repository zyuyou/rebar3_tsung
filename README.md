rebar3_tsung
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_tsung, ".*", {git, "git@host:user/rebar3_tsung.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_tsung
    ===> Fetching rebar3_tsung
    ===> Compiling rebar3_tsung
    <Plugin Output>
