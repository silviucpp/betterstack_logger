-module(betterstack_logger_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    start_handler/2,
    stop_handler/1,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_handler(Name, Opts) ->
    supervisor:start_child(?MODULE, [Name, Opts]).

stop_handler(Name) ->
    supervisor:terminate_child(?MODULE, Name).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 20,
        period => 1
    },

    ChildrenSpecs = #{
        id => betterstack_logger_h,
        start => {betterstack_logger_h, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [betterstack_logger_h]
    },

    {ok, {SupFlags, [ChildrenSpecs]}}.
