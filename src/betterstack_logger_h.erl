-module(betterstack_logger_h).

-define(PRINT_MSG(Format, Args),
    io:format(Format, Args)).

-behaviour(gen_server).

-export([
    start_link/2,

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(HACKNEY_POOL, betterstack_pool).
-define(BETTERSTACK_URL, <<"https://in.logs.betterstack.com">>).

-record(state, {
    hostname,
    formatter,

    extra_fields,
    bt_headers,

    upload_batch_max_size,
    upload_batch_inteval_ms,
    upload_failed_retry_count,
    upload_failed_retry_delay_ms,

    messages,
    msg_count,
    flush_timer
}).

start_link(Id, Opts) ->
    gen_server:start_link({local, Id}, ?MODULE, Opts, []).

init([Config, Formatter]) ->
    {ok, update_config(Config, Formatter, #state {
        hostname = get_hostname(),
        messages = [],
        msg_count = 0}
    )}.

handle_call({update_config, Config, Formatter}, _From, State) ->
    {reply, ok, update_config(Config, Formatter, State)};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({log, Msg}, State) ->
    {noreply, log_message(Msg, State)};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(push_messages, #state{msg_count = MsgCount} = State) ->
    case MsgCount > 0 of
        true ->
            {noreply, do_push_messages(State)};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    case Reason of
        {stop, shutdown} ->
            flush(State),
            wait_for_childrens(self(), 1000);
        _ ->
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internals

log_message(Msg, #state {
    messages = MessageList,
    msg_count = MessageCount,
    upload_batch_max_size = UploadBatchMaxSize,
    upload_batch_inteval_ms = UploadBatchMaxIntervalMs,
    flush_timer = FlushTimerRef
} = State) ->

    NewMessageCount = MessageCount + 1,

    case NewMessageCount >= UploadBatchMaxSize of
        true ->
            do_push_messages(State#state{messages = [Msg | MessageList], msg_count = NewMessageCount});
        _ ->
            NewTimer = case FlushTimerRef of
                undefined ->
                    erlang:send_after(UploadBatchMaxIntervalMs, self(), push_messages);
                _ ->
                    FlushTimerRef
            end,

            State#state{flush_timer = NewTimer, messages = [Msg | MessageList], msg_count = NewMessageCount}
    end.

do_push_messages(#state {
    upload_failed_retry_count = RetryCount,
    upload_failed_retry_delay_ms = RetryDelayMs,
    bt_headers = AuthHeaders,
    flush_timer = FlushTimerRef,
    formatter = Formatter,
    hostname = Hostname,
    extra_fields = ExtraFields,
    messages = Messages
} = State) ->

    case FlushTimerRef of
        undefined ->
            ok;
        _ ->
            erlang:cancel_timer(FlushTimerRef)
    end,

    spawn_link(fun() ->
        Payload = merge_json_payload(lists:map(fun(M) -> betterstack_encoder:encode(M, Hostname, ExtraFields, Formatter) end, Messages)),
        http_request(?BETTERSTACK_URL, post, Payload, AuthHeaders, RetryCount, RetryDelayMs)
    end),

    State#state{messages = [], msg_count = 0, flush_timer = undefined}.

flush(#state{msg_count = MsgCount} = State) ->
    case MsgCount > 0 of
        true ->
            ?PRINT_MSG("~p:flush_messages remaining messages: ~p, ~n", [?MODULE, MsgCount]),
            do_push_messages(State);
        _ ->
            ?PRINT_MSG("~p:flush_messages no remaining messages to push ~n", [?MODULE]),
            State
    end.

http_request(_Url, _Method, _PayloadJson, _AuthHeaders, 0, _IntervalMs) ->
    ok;
http_request(Url, Method, PayloadJson, AuthHeaders, Retries, IntervalMs) ->

    %?PRINT_MSG("#### Send: ~p ~n~n", [PayloadJson]),

    try
        Options = [
            {pool, ?HACKNEY_POOL}
        ],

        {ok, StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, Url, AuthHeaders, PayloadJson, Options),
        {ok, _Response} = hackney:body(ClientRef),

        case StatusCode of
            202 ->
                %?PRINT_MSG(<<"### Log sent response: ~p payload: ~p ~n">>, [Response, PayloadJson]),
                ok;
            _ ->
                ?PRINT_MSG("~p:http_request url: ~p failed status: ~p payload: ~p retries: ~p ~n", [?MODULE, Url, StatusCode, PayloadJson, Retries]),
                timer:sleep(IntervalMs),
                http_request(Url, Method, PayloadJson, AuthHeaders, Retries - 1, IntervalMs)
        end
    catch
        _ : Term ->
            ?PRINT_MSG("~p:http_request url: ~p exception: ~p payload: ~p retries: ~p ~n", [?MODULE, Url, Term, PayloadJson, Retries]),
            http_request(Url, Method, PayloadJson, AuthHeaders, Retries - 1, IntervalMs)
    end.

get_hostname() ->
    betterstack_utils:to_binary(string:trim(os:cmd("hostname -f"))).

wait_for_childrens(Pid, Timeout) ->
    {links, LinkedProcesses} = process_info(Pid, links),
    NumberChildren = length(LinkedProcesses) - 1,

    case NumberChildren > 0 of
        true ->
            ?PRINT_MSG("wait for childrens: ~p ~n", [NumberChildren]),
            timer:sleep(Timeout),
            wait_for_childrens(Pid, Timeout);
        _
            -> ok
    end.

create_http_pool(PoolOptions) ->
    case hackney_pool:find_pool(?HACKNEY_POOL) of
        undefined ->
            ok = hackney_pool:start_pool(?HACKNEY_POOL, PoolOptions);
        _ ->
            Timeout = proplists:get_value(timeout, PoolOptions, hackney_pool:timeout(?HACKNEY_POOL)),
            MaxConnections = proplists:get_value(max_connections, PoolOptions, hackney_pool:max_connections(?HACKNEY_POOL)),
            hackney_pool:set_timeout(?HACKNEY_POOL,Timeout),
            hackney_pool:set_max_connections(?HACKNEY_POOL, MaxConnections),
            ok
    end.

update_config(Config, Formatter, State) ->
    PoolOptions = maps:get(http_pool_options, Config, []),
    ok = create_http_pool(PoolOptions),

    State#state {
        formatter = Formatter,
        bt_headers = get_headers(maps:get(betterstack_source_token, Config)),
        extra_fields = maps:get(extra_fields, Config, []),
        upload_batch_max_size = maps:get(upload_batch_max_size, Config, 50),
        upload_batch_inteval_ms = maps:get(upload_batch_inteval_ms, Config, 5000),
        upload_failed_retry_count = maps:get(upload_failed_retry_count, Config, 3),
        upload_failed_retry_delay_ms = maps:get(upload_failed_retry_delay_ms, Config, 1000)
    }.

get_headers(SourceToken) ->
    [{<<"Content-type">>, <<"application/json">>}, {<<"Authorization">>, <<"Bearer ", SourceToken/binary>>}].

% we do this with a scope. for some reason some blob messages fails to encode in json . for those we encode the mesage
% as hex to make sure no log is lost: see betterstack_utils:safe_json_encode for details.

merge_json_payload([M]) ->
    M;
merge_json_payload([_|_] = V) ->
    <<"[", (betterstack_utils:join(V, <<",">>))/binary, "]">>.
