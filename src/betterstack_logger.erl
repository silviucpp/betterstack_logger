-module(betterstack_logger).

-export([
    adding_handler/1,
    changing_config/3,
    removing_handler/1,
    log/2
]).

adding_handler(#{id := Name, module := ?MODULE} = Config) ->
    Formatter = maps:get(formatter, Config, {logger_formatter, #{}}),
    HandlerConfig = maps:get(config, Config, #{}),
    case betterstack_logger_sup:start_handler(Name, [HandlerConfig, Formatter]) of
        {ok, _NewPid} ->
            ok;
        {error, {already_started, _OldPid}} ->
            gen_server:call(Name, {update_config, HandlerConfig, Formatter})
    end,
    {ok, Config}.

removing_handler(#{id := Name, module := ?MODULE}) ->
    betterstack_logger_sup:stop_handler(Name),
    ok.

changing_config(_SetOrUpdate, _OldConfig, #{id := Name, module := ?MODULE} = NewConfig) ->
    Formatter = maps:get(formatter, NewConfig, {logger_formatter, #{}}),
    HandlerConfig = maps:get(config, NewConfig, #{}),
    case gen_server:call(Name, {update_config, HandlerConfig, Formatter}) of
        ok ->
            {ok, NewConfig};
        Error ->
            Error
    end.

log(LogEvent, #{id := Name, module := ?MODULE}) ->
    gen_server:cast(Name, {log, LogEvent}).

