%%%-------------------------------------------------------------------
%%% @author Wajktor13
%%% @copyright (C) 2023, Wiktor Wilkusz
%%% @doc
%%%
%%% @end
%%% Created : 29. May 2023 11:25
%%%-------------------------------------------------------------------
-module(pollution).
-author("Wiktor").

-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3,
    get_daily_mean/3, station_exceeded_limit_on_date/4, get_daily_over_limit/4, station_exists/3]).

-record(station, {name, coordinates, measurements}).


%% helpers

validate_coordinates({X, Y}) when is_number(X) and is_number(Y) -> true;
validate_coordinates(_) -> false.

get_station_by_name_or_coords([], _, _) -> false;
get_station_by_name_or_coords([H | T], Name, Coordinates) ->
    if
        (H#station.name == Name) or (H#station.coordinates == Coordinates) -> H;
        true -> get_station_by_name_or_coords(T, Name, Coordinates)
    end.


get_single_station_measurements_on_datetime(Station, DateTime) ->
    case maps:find(DateTime, Station#station.measurements) of
        error -> false;
        {ok, MeasurementsOnDateTime} -> MeasurementsOnDateTime
    end.


add_measurements_set_on_datetime(Station, DateTime, Type, Value, Monitor) ->
    NewMeasurementsSet = maps:put(DateTime, maps:put(Type, Value, maps:new()), Station#station.measurements),
    [Station#station{measurements = NewMeasurementsSet} | lists:delete(Station, Monitor)].


add_measurement_on_datetime(Station, DateTime, Type, Value, Monitor, MeasurementsOnDateTime) ->
    [Station#station{measurements = maps:update(DateTime, maps:put(Type, Value, MeasurementsOnDateTime),
        Station#station.measurements)} | lists:delete(Station, Monitor)].


measurement_exists(Type, MeasurementsOnDateTime) ->
    case maps:find(Type, MeasurementsOnDateTime) of
        error -> false;
        _ -> true
    end.


validate_new_measurement({{Y, M, D}, {H, MI, S}}, Type, Value) ->
    is_integer(Y) and is_integer(M) and is_integer(D) and is_integer(H) and is_integer(MI) and is_integer(S) and
        (string:equal(Type, "PM1") or string:equal(Type, "PM10") or string:equal(Type, "PM25")) and is_number(Value).


remove_measurement_on_datetime(Station, DateTime, Type, MeasurementsOnDateTime, Monitor) ->
    UpdatedMeasurementsSet = maps:update(DateTime, maps:remove(Type, MeasurementsOnDateTime),
        Station#station.measurements),
    [Station#station{measurements = UpdatedMeasurementsSet} | lists:delete(Station, Monitor)].


get_mean_of_list([]) ->
    io:format("[pollution] cannot get mean of empty list.~n"),
    {error, mean_of_empty_list};
get_mean_of_list(A) ->
    lists:sum(A) / length(A).


get_measurements_on_date_all_stations(Monitor, Date) ->
    AllMeasurements = [MeasurementsOnDate || Station <- Monitor,
        MeasurementsOnDate <- maps:to_list(Station#station.measurements)],
    lists:filter(fun ({{D, _}, _}) -> D == Date end, AllMeasurements).


get_measurements_on_date_single_station(Station, Date) ->
    AllMeasurements = [MeasurementsOnDate || MeasurementsOnDate <- maps:to_list(Station#station.measurements)],
    lists:filter(fun ({{D, _}, _}) -> D == Date end, AllMeasurements).


station_exceeded_limit_on_date(Station, Date, Type, Limit) ->
    MeasurementsOnDate = [maps:to_list(Measurement) || {_, Measurement} <-
        get_measurements_on_date_single_station(Station, Date)],
    MeasurementsOnDateOfType = lists:filter(fun ([{T, _}]) -> string:equal(Type, T) end, MeasurementsOnDate),
    lists:foldl(fun (V, R) -> (V > Limit) or R end, false, [V || [{_, V}] <- MeasurementsOnDateOfType]).


get_all_measurements_from_station_of_type(Station, Type) ->
    AllMeasurementsOfStation = [maps:to_list(AllMeasurements) || AllMeasurements
        <- maps:values(Station#station.measurements)],
    lists:filter(fun ([{T, _}]) -> string:equal(Type, T) end, AllMeasurementsOfStation).


%% API

create_monitor() -> [].


add_station(Name, Coordinates, Monitor) ->
    case station_exists(Monitor, Name, Coordinates) of
        true ->
            io:format("[pollution] cannot add new station. Station with such coordinates or name already exists.~n"),
            {error, station_already_exists};
        false ->
            case validate_coordinates(Coordinates) of
                true -> [#station{name = Name, coordinates = Coordinates, measurements = maps:new()} | Monitor];
                false ->
                    io:format("[pollution] cannot add new station. Coordinates are incorrect.~n"),
                    {error, incorrect_coordinates}
            end
    end.


add_value(StationKey, DateTime, Type, Value, Monitor) ->
    case get_station_by_name_or_coords(Monitor, StationKey, StationKey) of
        false ->
            io:format("[pollution] cannot add new measurement. Station with such coordinates or name does not exists.~n"),
            {error, station_does_not_exist};
        Station ->
            case validate_new_measurement(DateTime, Type, Value) of
                true ->
                    case get_single_station_measurements_on_datetime(Station, DateTime) of
                        false -> add_measurements_set_on_datetime(Station, DateTime, Type, Value, Monitor);
                        MeasurementsOnDateTime ->
                            case measurement_exists(Type, MeasurementsOnDateTime) of
                                false -> add_measurement_on_datetime(Station, DateTime, Type, Value, Monitor,
                                    MeasurementsOnDateTime);
                                true ->
                                    io:format("[pollution] cannot add new measurement. Measurement with the same date and type already exists.~n"),
                                    {error, measurement_already_exists}
                            end
                    end;
                false ->
                    io:format("[pollution] cannot add new measurement. Measurement data is incorrect.~n"),
                    {error, measurement_data_incorrect}
            end

    end.


remove_value(StationKey, DateTime, Type, Monitor) ->
    case get_station_by_name_or_coords(Monitor, StationKey, StationKey) of
        false ->
            io:format("[pollution] cannot remove measurement. Station with such coordinates or name does not exists.~n"),
            {error, station_does_not_exist};
        Station ->
            case get_single_station_measurements_on_datetime(Station, DateTime) of
                false ->
                    io:format("[pollution] cannot remove measurement. There are no measurements made on provided time.~n"),
                    {error, measurement_does_not_exist};
                MeasurementsOnDateTime ->
                    case measurement_exists(Type, MeasurementsOnDateTime) of
                        false ->
                            io:format("[pollution] cannot remove measurement. Measurement of this type does not exist on provided station and time.~n"),
                            {error, measurement_does_not_exist};
                        true -> remove_measurement_on_datetime(Station, DateTime, Type, MeasurementsOnDateTime, Monitor)
                    end
            end
    end.


get_one_value(StationKey, DateTime, Type, Monitor) ->
    case get_station_by_name_or_coords(Monitor, StationKey, StationKey) of
        false ->
            io:format("[pollution] cannot get measurement value. Station with such coordinates or name does not exists.~n"),
            {error, station_does_not_exist};
        Station ->
            case get_single_station_measurements_on_datetime(Station, DateTime) of
                false ->
                    io:format("[pollution] cannot get measurement value. There are no measurements made on provided station and time.~n"),
                    {error, measurement_does_not_exist};
                MeasurementsOnDateTime ->
                    case maps:find(Type, MeasurementsOnDateTime) of
                        error ->
                            io:format("[pollution] cannot get measurement value. Measurement of this type does not exist on provided station and time.~n"),
                            {error, measurement_does_not_exist};
                        {ok, Value} -> Value
                    end
            end
    end.


get_station_mean(StationKey, Type, Monitor) ->
    case get_station_by_name_or_coords(Monitor, StationKey, StationKey) of
        false ->
            io:format("[pollution] cannot get station mean. Station with such coordinates or name does not exists.~n"),
            {error, station_does_not_exist};
        Station ->
            case get_all_measurements_from_station_of_type(Station, Type) of
                [] ->
                    io:format("[pollution] cannot get station mean. Station with such coordinates or name has no measurements.~n"),
                    {error, station_has_no_measurements};
                Measurements -> get_mean_of_list([V || [{_, V}] <- Measurements])
            end
    end.


get_daily_mean(Type, Date, Monitor) ->
    get_mean_of_list([V || {T, V} <- [lists:nth(1, maps:to_list(Measurement)) ||
        {_, Measurement} <- get_measurements_on_date_all_stations(Monitor, Date)], string:equal(Type, T)]).


station_exists(Monitor, Name, Coordinates) ->
    case get_station_by_name_or_coords(Monitor, Name, Coordinates) of
        false -> false;
        _     -> true
    end.


%% individual task

get_daily_over_limit(_, _, _, []) -> 0;
get_daily_over_limit(Date, Type, Limit, [S | T]) ->
    case station_exceeded_limit_on_date(S, Date, Type, Limit) of
        true -> 1 + get_daily_over_limit(Date, Type, Limit, T);
        false -> get_daily_over_limit(Date, Type, Limit, T)
    end.
