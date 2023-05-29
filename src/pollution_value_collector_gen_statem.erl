%%%-------------------------------------------------------------------
%%% @author Wajktor13
%%% @copyright (C) 2023, Wiktor Wilkusz
%%% @doc
%%%
%%% @end
%%% Created : 29. May 2023 16:42
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).

-author("Wajktor13").

-behaviour(gen_statem).

-export([init/1, callback_mode/0, start_link/0, terminate/2]).
-export([set_station/1, add_value/3, store_data/0, stop/0]).
-export([waiting_for_station_declaration/3, collecting_data/3]).


start_link() ->
    io:format("[pollution_value_collector_gen_statem] server started~n"),
    gen_statem:start_link(
        {local, pollution_value_collector_gen_statem},
        ?MODULE,
        {},
        []
    ).


init({}) ->
    {ok, waiting_for_station_declaration, {}}.


terminate(Reason, _) ->
    io:format("[pollution_value_collector_gen_statem] shutdown. Reason: ~p~n", [Reason]).


callback_mode() ->
    state_functions.

% API

set_station(StationKey) ->
    gen_statem:cast(pollution_value_collector_gen_statem, {set_station, StationKey}).


add_value(DateTime, Type, Value) ->
    gen_statem:cast(pollution_value_collector_gen_statem, {collect_data, DateTime, Type, Value}).


store_data() ->
    gen_statem:cast(pollution_value_collector_gen_statem, {store_data}).


stop() ->
    gen_server:cast(pollution_gen_server, stop).

% Handlers

waiting_for_station_declaration(_Event, {set_station, StationKey}, {}) ->
    case pollution:station_exists(pollution_gen_server:get_monitor(), StationKey, StationKey) of
        true -> io:format("[pollution_value_collector_gen_statem] station has been successfully set~n"),
            {next_state, collecting_data, {StationKey, []}};
        false -> io:format("[pollution_value_collector_gen_statem] station with provided name or coords does not exist~n"),
            {next_state, waiting_for_station_declaration, {}}
    end.


collecting_data(_Event, {collect_data, DateTime, Type, Value}, {StationKey, Data}) ->
    io:format("[pollution_value_collector_gen_statem] data has been added~n"),
    {next_state, collecting_data, {StationKey, [{DateTime, Type, Value} | Data]}};

collecting_data(_Event, {store_data}, {StationKey, Data}) ->
    lists:map(fun ({DateTime, Type, Value}) -> pollution_gen_server:add_value(StationKey, DateTime, Type, Value) end,
        Data),
    io:format("[pollution_value_collector_gen_statem] data has been added to station. Waiting for new station...~n"),
    {next_state, waiting_for_station_declaration, {}}.
