defmodule Parser do
    def parse_line(line) do
        [date, time, lo, la, value] = line |> String.split(",")
        [day, month, year] = date |> String.split("-")
        [hour, minute] = time |> String.split(":")

        %{
            :datetime =>
                {{parse_int(year), parse_int(month), parse_int(day)}, {parse_int(hour), parse_int(minute), 0}},
            :location => {parse_float(lo), parse_float(la)},
            :pollutionLevel => parse_int(value)
        }
    end

    def parse_int(int) do
        int |> Integer.parse() |> elem(0)
    end

    def parse_float(float) do
        float |> Float.parse() |> elem(0)
    end
end


defmodule StationsIdentifier do
    def identify_stations(data) do
        unique_stations = Enum.uniq_by(data, fn d -> d.location end)
        Enum.map(unique_stations, fn d -> d.location end)
    end
end


defmodule NameGenerator do
    def generate_name({lo, la}) do
        "station_#{lo}_#{la}"
    end
end


defmodule DataLoader do
    def load_data do
        data_path = "C:/Users/Wiktor/Downloads/pollution.csv"
        data = File.read!(data_path) |> String.split("\n")

        parsed_data = Enum.map(data, fn line -> Parser.parse_line(line) end)

        unique_stations_locations = StationsIdentifier.identifyStations(parsed_data)

        Enum.map(unique_stations_locations, fn location ->
            :pollution_gen_server.add_station(NameGenerator.generate_name(location), location)
        end)

        Enum.map(parsed_data, fn data ->
            :pollution_value_collector_gen_statem.set_station(Map.get(data, :location))

            :pollution_value_collector_gen_statem.add_value(
                Map.get(data, :datetime),
                "PM10",
                Map.get(data, :pollutionLevel)
            )

            :pollution_value_collector_gen_statem.store_data()
        end)
    end
end



