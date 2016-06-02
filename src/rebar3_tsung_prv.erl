-module(rebar3_tsung_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, tsung).
-define(DEPS, [app_discovery, lock]).

-define(TSUNG_COMMAND, "tsung").
-define(TSUNG_DTD_ELEMENT_STRUCTURE,
    "\<!ELEMENT ~s ~s >~n"
    "<!ATTLIST ~s"
    "~s>~n~n"
).
-define(TSUNG_DTD_ELEMENT_ATTR_STRUCTURE, "~n   ~s ~s ~s").

-define(RUN_TSUNG_TIMEOUT, 300000).

-record(tsung_args, {
    root = "tsung",

    plugin_pa,

    config,
    dtd,
    log_dir,
    extra_pa = []
}).

-record(ts_plugin, {
    name,
    fiels,
    child
}).

-record(ts_plugin_attr, {
    name,
    type,
    default
}).

-record(template_vars, {
    session_typs = [],
    request_types = [],
    request_types_define = []
}).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {module, ?MODULE},            % The module implementation of the task
        {bare, true},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 rebar3_tsung"}, % How to use the plugin
        {opts, []},                   % list of options understood by the plugin
        {short_desc, "A rebar plugin"},
        {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    TsungConfig = rebar_state:get(State, tsung, [
        {root, "tsung"}
    ]),

    {root, Root} = proplists:get_value(root, TsungConfig, {root, "tsung"}),

    case strip_flags(rebar_state:command_args(State)) of
        ["help"] ->
            io:format("Call `rebar3 tsung` to run tsung test~n", []),
            {ok, State};
        ["new"] ->
            io:format("new ~n", []),
            io:format("new ~p~n", [rebar_state:all_deps(State)]),
            {ok, State};
        ["new", "dtd"] ->
            case lists:keyfind(plugins, 1, TsungConfig) of
                false ->
                    io:format("new dtd empty skip, ~n", []),
                    {ok, State};
                {plugins, Plugins} ->
                    TsPlugins = parse_plugins(Plugins),
                    #template_vars{
                        session_typs = SessionTypes,
                        request_types = RequestTypes,
                        request_types_define = RequestTypesDefine
                    } = _TemplateVars = gen_template_args(TsPlugins, #template_vars{}),

                    Opts = [
                        {session_types, lists:flatten(["| " ++ atom_to_list(T) || T <- SessionTypes])},
                        {request_types, lists:flatten(["| " ++ atom_to_list(T) || T <- RequestTypes])},
                        {request_types_define, lists:flatten(RequestTypesDefine)},
                        {output, Root}
                    ],

                    ok = rebar_templater:new("tsung_dtd", Opts, true, State),
                    {ok, State}
            end;
        [] ->
            %% tsung plugin project apps ebin path
            TsungArgs = tsung_args(TsungConfig, #tsung_args{plugin_pa = plugin_project_pas(State)}),

            Timeout = proplists:get_value(timeout, TsungConfig, ?RUN_TSUNG_TIMEOUT),

            Command = parse_command(TsungArgs),
            case run_tsung(Command, Timeout) of
                {ok, _} ->
                    {ok, State};
                Error ->
                    {error, {?MODULE, Error}}
            end
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

plugin_project_pas(State) ->
    lists:foldl(
        fun(AppInfo, Acc) ->
            case rebar_app_info:name(AppInfo) of
                <<"tsung_plugin_helper">> ->
                    Acc;
                _AppName ->
                    [rebar_app_info:ebin_dir(AppInfo) | Acc]
            end
        end, [], rebar_state:project_apps(State) ++ rebar_state:all_deps(State)).

strip_flags([]) -> [];
strip_flags(["-"++_|Opts]) -> strip_flags(Opts);
strip_flags([Opt | Opts]) -> [Opt | strip_flags(Opts)].

tsung_args([], #tsung_args{} = Args) ->
    Args;
tsung_args([{root, Root} | Rest], #tsung_args{} = Args) ->
    tsung_args(Rest, Args#tsung_args{root = Root});
tsung_args([{config, Config} | Rest], #tsung_args{} = Args) ->
    tsung_args(Rest, Args#tsung_args{config = Config});
tsung_args([{dtd, DtdFile} | Rest], #tsung_args{} = Args) ->
    tsung_args(Rest, Args#tsung_args{dtd = DtdFile});
tsung_args([{log_dir, LogDir} | Rest], #tsung_args{} = Args) ->
    tsung_args(Rest, Args#tsung_args{log_dir = LogDir});
tsung_args([{extra_pa, ExtraPA} | Rest], #tsung_args{extra_pa = OldExtraPA} = Args) ->
    tsung_args(Rest, Args#tsung_args{extra_pa = ExtraPA ++ OldExtraPA});
tsung_args([_H | Rest], #tsung_args{} = Args) ->
    tsung_args(Rest, Args).

parse_command(#tsung_args{root = Root} = Args) ->
    ParamsTsung = [?TSUNG_COMMAND],

    ParamsConfig =
        case Args#tsung_args.config of
            undefined ->
                [filename:join(Root, "tsung.xml"), "-f" | ParamsTsung];
            Config ->
                [Config, "-f" | ParamsTsung]
        end,

    ParamsLog =
        case Args#tsung_args.log_dir of
            undefined ->
                [filename:join(Root, "log"), "-l" | ParamsConfig];
            LogDir ->
                [LogDir, "-l" | ParamsConfig]
        end,

    ParamsPluginPa =
        lists:foldl(
            fun(Path, Acc) ->
                [Path, "-X" | Acc]
            end, ParamsLog, Args#tsung_args.plugin_pa ++ Args#tsung_args.extra_pa),

    string:join(lists:reverse(["start" | ParamsPluginPa]), " ").

run_tsung(Command, Timeout) ->
%%    io:format("Tsung run command:~p~n", [Command]),
    Port = erlang:open_port({spawn, Command}, [exit_status]),
    tsung_loop(Port, [], Timeout).

tsung_loop(Port, Data, Timeout) ->
    receive
        {Port, {data, MoreData}} ->
            rebar_api:console(MoreData, []),
            tsung_loop(Port, MoreData ++ Data, Timeout);
        {Port, {exit_status, 0}} ->
            {ok, Data};
        {Port, {exit_status, Error}} ->
            throw({tsung_fail, Error})
    after
        Timeout ->
            throw(timeout)
    end.

%%====================
%% parse plugins
%%====================
gen_template_args([], Vars) ->
    Vars;
gen_template_args([#ts_plugin{} = Plugin | Rest], #template_vars{} = Vars) ->
    #template_vars{
        session_typs = OldSessionTypes,
        request_types = OldRequestTypes,
        request_types_define = OldRequestTypesDefine
    } = Vars,

    VarsNew = #template_vars{
        session_typs = [list_to_atom("ts_" ++ atom_to_list(Plugin#ts_plugin.name)) | OldSessionTypes],
        request_types = [Plugin#ts_plugin.name | OldRequestTypes],
        request_types_define = [gen_define_str(Plugin) | OldRequestTypesDefine]
    },
    gen_template_args(Rest, VarsNew);
gen_template_args([_Plugin | Rest], Vars) ->
    gen_template_args(Rest, Vars).

gen_define_str(#ts_plugin{name = PluginName} = Plugin) ->
    Child =
        case Plugin#ts_plugin.child of
            true ->
                "(#PCDATA)";
            false ->
                "EMPTY"
        end,

    Attrs =
        lists:foldl(
            fun(#ts_plugin_attr{name = Name, type = Type, default = Default}, AttrAcc) ->
                AttrType =
                    case Type of
                        Type when is_list(Type) ->
                            "(" ++ string:join([atom_to_list(T) || T <- Type], "|") ++ ")";
                        Type when is_atom(Type) ->
                            string:to_upper(atom_to_list(Type))
                    end,

                AttrDefault =
                    case Default of
                        Default when is_list(Default) ->
                            Default;
                        Default when is_atom(Default) ->
                            "#" ++ string:to_upper(atom_to_list(Default))
                    end,

                io_lib:format(?TSUNG_DTD_ELEMENT_ATTR_STRUCTURE, [Name, AttrType, AttrDefault]) ++ AttrAcc
            end, "", Plugin#ts_plugin.fiels),

    io_lib:format(?TSUNG_DTD_ELEMENT_STRUCTURE, [PluginName, Child, PluginName, Attrs]).

parse_plugins(Plugins) ->
    parse_plugins(Plugins, []).

parse_plugins([], Acc) ->
    Acc;
parse_plugins([{PluginName, Fields, Child} | Rest], Acc) ->
    TsPlugin = #ts_plugin{
        name = PluginName,
        fiels = parse_plugin_attrs(Fields, []),
        child = Child
    },
    parse_plugins(Rest, [TsPlugin | Acc]);
parse_plugins([{PluginName, Fields}| Rest], Acc) ->
    parse_plugins([{PluginName, Fields, true}| Rest], Acc);
parse_plugins([_Other | Rest], Acc) ->
    parse_plugins(Rest, Acc).

parse_plugin_attrs([], Acc) ->
    Acc;
parse_plugin_attrs([{FieldName, FieldType, FieldDefault}|Rest], Acc) ->
    Field = #ts_plugin_attr{
        name = FieldName,
        type = FieldType,
        default = FieldDefault
    },
    parse_plugin_attrs(Rest, [Field|Acc]);
parse_plugin_attrs([_Other | Rest], Acc) ->
    parse_plugin_attrs(Rest, Acc).
