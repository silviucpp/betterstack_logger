-module(betterstack_utils).

-export([
    safe_json_encode/1,
    to_binary/1,
    join/2
]).

safe_json_encode(Msg) ->
    case catch jiffy:encode({Msg}, [force_utf8]) of
        JsonPayloadBin when is_binary(JsonPayloadBin) ->
            JsonPayloadBin;
        JsonPayloadList when is_list(JsonPayloadList) ->
            iolist_to_binary(JsonPayloadList);
        {error, _} ->
            {value, {_, InnerMsg}, Msg1} = lists:keytake(<<"message">>, 1, Msg),
            InnerMsg2 = iolist_to_binary(io_lib:format("hex msg. json encode failed: ~p", [binary:encode_hex(term_to_binary(InnerMsg))])),
            safe_json_encode([{<<"message">>, InnerMsg2} | Msg1])
    end.

to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_list(V) ->
    list_to_binary(V);
to_binary(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) ->
    integer_to_binary(V);
to_binary(V) when is_float(V) ->
    float_to_bin(V);
to_binary(V) when is_pid(V) ->
    list_to_binary(pid_to_list(V)).

float_to_bin(Value) ->
    Truncated = trunc(Value),
    case Truncated == Value of
        true ->
            integer_to_binary(Truncated);
        _ ->
            float_to_binary(Value, [{decimals, 4}, compact])
    end.

join([Head | Tail], Sep) ->
    join_list_sep(Tail, Sep, [Head]);
join([], _Sep) ->
    <<>>.

% internals

join_list_sep([Head | Tail], Sep, Acc) ->
    join_list_sep(Tail, Sep, [Head, Sep | Acc]);
join_list_sep([], _Sep, Acc) ->
    list_to_binary(lists:reverse(Acc)).
