rebar3_live
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your global rebar config ~/.config/rebar3/rebar.conf:

    {plugins, [rebar3_live]}.

Then just call your plugin directly in an existing application:

    $ rebar3 live
    ===> Fetching rebar3_live
    ===> Compiling rebar3_live
    <Plugin Output>

Issues
---

Better to install globally. Plugin crashes when installed locally and project
fails to compile.
