-module(betterstack_encoder).

-export([
    encode/4
]).

% no_formatter and hostname inside event are related to some internal apps.

encode(#{level := Level, meta := Meta} = Event, Hostname, ExtraFields, Formatter) ->
    betterstack_utils:safe_json_encode([
        {<<"dt">>, erlang:trunc(maps:get(time, Meta)/1000)},
        {<<"level">>, severity2bin(Level)},
        {<<"severity">>, severity2int(Level)},
        {<<"host">>, maps:get(hostname, Event, Hostname)},
        {<<"message">>, format_message(Event, Formatter)} | ExtraFields
    ]).

% internals

format_message(Event, {FormatterModule, FormatterConfig}) ->
    case maps:get(no_formatter, Event, false) of
        true ->
            maps:get(msg, Event);
        _ ->
            betterstack_utils:to_binary(FormatterModule:format(Event, FormatterConfig))
    end.

severity2bin(debug) ->
    <<"debug">>;
severity2bin(info) ->
    <<"info">>;
severity2bin(notice) ->
    <<"notice">>;
severity2bin(warning) ->
    <<"warning">>;
severity2bin(error) ->
    <<"error">>;
severity2bin(critical) ->
    <<"critical">>;
severity2bin(alert) ->
    <<"alert">>;
severity2bin(emergency) ->
    <<"emergency">>;
severity2bin(_) ->
    <<"debug">>.

severity2int(debug) ->
    7;
severity2int(info) ->
    6;
severity2int(notice) ->
    5;
severity2int(warning) ->
    4;
severity2int(error) ->
    3;
severity2int(critical) ->
    2;
severity2int(alert) ->
    1;
severity2int(emergency) ->
    0;
severity2int(_) ->
    7.
