%%%-------------------------------------------------------------------
%% @doc pollution top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link(
        {local, ?SERVER},
        ?MODULE,
        []
    ).

init(InitialValue) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 3},
    ChildSpecs = [
        #{id => 'pollution_server',
            start => {pollution_gen_server, start_link, [InitialValue]},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [pollution_gen_server, pollution]},
        #{id => 'pollution_value_collector',
            start => {pollution_value_collector_gen_statem, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [pollution_value_collector_gen_statem, pollution_gen_server, pollution]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
