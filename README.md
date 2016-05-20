rebar3_tsung
=====

A rebar plugin for helping develop Tsung plugin

Build
-----

    $ rebar3 compile

Use
---

`rebar3 new` a lib project, Add the plugin to your rebar config:

    {plugins, [
        { rebar3_tsung, ".*", {git, "git@host:user/rebar3_tsung.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 tsung
    ===> Fetching rebar3_tsung
    ===> Compiling rebar3_tsung
    <Plugin Output>

Exmaple
---
Add `tsung config` to rebar.config.
```
{tsung, [
%%    {root, "tsung"},          % default is "tsung"
%%    {config, "tsung.xml"},    % default is {{root}}/tsung.xml
%%    {dtd, "tsung-1.0.dtd"},   % default is {{root}}/tsung-1.0.dtd
%%    {log_dir, "log"}          % default is {{root}}/log

    {plugins, [
        % {PluginName :: atom(), Attrs :: list(Attr()), IsEmpty:: bool()}
        % Attr() :: {AttrName:: atom(), CDATA | list(enum()), Default},
        % enum() :: atom()
        {foo, [
            {type, [connect, bar], required},
            {msg, cdata, implied}
        ]}
    ]}
]}.
```

> Call `rebar3 tsung` will run `tsung ...args start` which has flag like `-X _build/default/lib/{{tsung_plugin_lib}}/ebin`.

> Call `rebar3 tsung new dtd` will generate an dtd-file from `plugins` config, some code would like below:

```
<!ATTLIST session
    name         CDATA #REQUIRED
    bidi         CDATA #IMPLIED
    persistent   (true | false) #IMPLIED
    probability   NMTOKEN #IMPLIED
    weight        NMTOKEN #IMPLIED
    type         (ts_jabber | ts_http | ts_raw | ts_pgsql | ts_ldap | ts_webdav |ts_mysql| ts_fs | ts_shell | ts_job | ts_websocket | ts_amqp | ts_mqtt | **ts_foo**) #REQUIRED>
```

```
<!ELEMENT change_type EMPTY>
<!ATTLIST change_type
     new_type         (ts_jabber | ts_http | ts_raw | ts_pgsql | ts_ldap | ts_webdav | ts_mysql | ts_fs | ts_shell | ts_job | ts_websocket | ts_amqp | ts_mqtt | **ts_sango**) #REQUIRED
     host NMTOKEN #REQUIRED
     port NMTOKEN #REQUIRED
     server_type NMTOKEN #REQUIRED
     store  ( true | false ) "false"
     restore ( true | false ) "false"
     bidi ( true | false ) "false"
    >
```

```
<!ELEMENT foo (#PCDATA) >
<!ATTLIST foo
   type (connect | bar) #REQUIRED
   msg CDATA #IMPLIED>
```



