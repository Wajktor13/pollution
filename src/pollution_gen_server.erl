%%%-------------------------------------------------------------------
%%% @author Wajktor13
%%% @copyright (C) 2023, Wiktor Wilkusz
%%% @doc
%%%
%%% @end
%%% Created : 29. May 2023 13:49
%%%-------------------------------------------------------------------

-module(pollution_gen_server).

-author("Wajktor13").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([get_monitor/0, add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2,
    get_daily_mean/2, get_daily_over_limit/3, crash/0, stop/0]).


start_link(InitialValue) ->
    io:format("[pollution_gen_server] server started~n"),
    gen_server:start_link(
        {local, ?SERVER},
        ?MODULE,
        InitialValue,
        []
    ).


init(_) ->
    {ok, pollution:create_monitor()}.


terminate(Reason, _) ->
    io:format("[pollution_gen_server] shutdown. Reason: ~p~n", [Reason]).

% API

get_monitor() ->
    gen_server:call(pollution_gen_server, {get_monitor}).


add_station(Name, Coordinates) ->
    gen_server:cast(pollution_gen_server, {add_station, Name, Coordinates}).


add_value(StationKey, DateTime, Type, Value) ->
    gen_server:cast(pollution_gen_server, {add_value, StationKey, DateTime, Type, Value}).


remove_value(StationKey, DateTime, Type) ->
    gen_server:cast(pollution_gen_server, {remove_value, StationKey, DateTime, Type}).


get_one_value(StationKey, DateTime, Type) ->
    gen_server:call(pollution_gen_server, {get_one_value, StationKey, DateTime, Type}).


get_station_mean(StationKey, Type) ->
    gen_server:call(pollution_gen_server, {get_station_mean, StationKey, Type}).


get_daily_mean(Type, Date) ->
    gen_server:call(pollution_gen_server, {get_daily_mean, Type, Date}).


get_daily_over_limit(Date, Type, Limit) ->
    gen_server:call(pollution_gen_server, {get_daily_over_limit, Date, Type, Limit}).


crash() ->
    gen_server:cast(pollution_gen_server, {crash}).


stop() ->
    gen_server:cast(pollution_gen_server, stop).

% call handlers

handle_call({get_monitor}, _From, Monitor) ->
    io:format("[pollution_gen_server] monitor:~n"),
    {reply, Monitor, Monitor};


handle_call({get_one_value, StationKey, DateTime, Type}, _From, Monitor) ->
    io:format("[pollution_gen_server] one value:~n"),
    {reply, pollution:get_one_value(StationKey, DateTime, Type, Monitor), Monitor};


handle_call({get_station_mean, StationKey, Type}, _From, Monitor) ->
    io:format("[pollution_gen_server] station mean:~n"),
    {reply, pollution:get_station_mean(StationKey, Type, Monitor), Monitor};


handle_call({get_daily_mean, Type, Date}, _From, Monitor) ->
    io:format("[pollution_gen_server] daily mean:~n"),
    {reply, pollution:get_daily_mean(Type, Date, Monitor), Monitor};


handle_call({get_daily_over_limit, Date, Type, Limit}, _From, Monitor) ->
    io:format("[pollution_gen_server] daily over limit:~n"),
    {reply, pollution:get_daily_over_limit(Date, Type, Limit, Monitor), Monitor}.

% cast handlers

handle_cast({add_station, Name, Coordinates}, Monitor) ->
    case pollution:add_station(Name, Coordinates, Monitor) of
        {error, _} -> {noreply, Monitor};
        NewMonitor -> io:format("[pollution_gen_server] successfully added new station~n"),
            {noreply, NewMonitor}
    end;


handle_cast({add_value, StationKey, DateTime, Type, Value}, Monitor) ->
    case pollution:add_value(StationKey, DateTime, Type, Value, Monitor) of
        {error, _} -> {noreply, Monitor};
        UpdatedMonitor -> io:format("[pollution_gen_server] successfully added new value~n"),
            {noreply, UpdatedMonitor}
    end;


handle_cast({remove_value, StationKey, DateTime, Type}, Monitor) ->
    case pollution:remove_value(StationKey, DateTime, Type, Monitor) of
        {error, _} -> {noreply, Monitor};
        UpdatedMonitor -> io:format("[pollution_gen_server] successfully removed value~n"),
            {noreply, UpdatedMonitor}
    end;


handle_cast({crash}, _) ->
    exit(crash);


handle_cast(stop, Monitor) ->
    {stop, normal, Monitor}.
