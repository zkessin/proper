-module(prop_car).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3, list_commands/1, num_commands/0]).

-type fuel() :: float().
-type speed() :: non_neg_integer().
-type distance() :: float().

-record(state, {fuel :: fuel(),
                speed :: speed(),
                distance :: distance(),
                burnt :: float()}).

-define(SERVER, targeted_statem_car).

-include("targeted_statem_car.hrl").

%% ---------------------------------------------------------------------------
%% Generators
%% ---------------------------------------------------------------------------

% accelerate(Speed) when Speed > 20 ->
%     integer(10, round(Speed * 0.1));
accelerate(Speed) ->
    integer(0, 200 - Speed).

% brake(Speed) when Speed > 20 ->
%     integer(10, round(Speed * 0.1));
brake(Speed) ->
    integer(0, Speed).

refuel(Fuel) ->
    integer(0, round(?MAX_FUEL - Fuel)).

%% ----------------------------------------------------------------------------
%% statem callbacks
%% ----------------------------------------------------------------------------

initial_state() ->
    #state{
        fuel = ?MAX_FUEL,
        speed = 0,
        burnt = 0,
        distance = 0
    }.

command(S) ->
    #state{fuel = Fuel, speed = Speed} = S,
    Normal = [
        {1, {call, ?SERVER, accelerate, [accelerate(Speed)]}},
        {1, {call, ?SERVER, refuel, [refuel(Fuel)]}}
    ],
    End = Normal ++ [
        {10, {call, ?SERVER, travel, [integer(20, 100)]}} || Speed > 40.0 andalso Speed < 100.0
    ] ++ [
        {2, {call, ?SERVER, travel, [integer(10, 50)]}} || Speed >= 100.0
    ] ++ [
        {10, {call, ?SERVER, brake, [brake(Speed)]}} || Speed >= 100.0
    ] ++ [
        {2, {call, ?SERVER, brake, [brake(Speed)]}} || Speed < 100.0 andalso Speed > 0.0
    ],
    frequency(End).

list_commands(S) ->
    Speed = S#state.speed,
    Fuel = S#state.fuel,
    [
        {call, ?SERVER, accelerate, [accelerate(Speed)]},
        {call, ?SERVER, brake, [brake(Speed)]},
        {call, ?SERVER, travel, [integer(1, 100)]},
        {call, ?SERVER, refuel, [refuel(Fuel)]}
    ].

num_commands() -> 4.

precondition(#state{fuel = Fuel, speed = Speed}, {call, _, accelerate, _}) ->
    Fuel > ?MAX_FUEL * 0.1 andalso Speed < 190;
precondition(#state{speed = Speed}, {call, _, brake, _}) ->
    Speed > 1;
precondition(#state{speed = Speed}, {call, _, travel, _}) ->
    Speed > 1;
precondition(#state{fuel = Fuel}, {call, _, refuel, _}) ->
    Fuel < ?MAX_FUEL * 0.8;
precondition(_, _) ->
    true.

postcondition(_S, _, {Distance, _Consumptions}) ->
    Distance >= 0.0.

next_state(S, _V, {call, _, accelerate, [Value]}) ->
    #state{
        fuel = Fuel,
        speed = Speed,
        distance = Distance,
        burnt = B
    } = S,
    {Travelled, Acceleration, _, Burnt} = acceleration_calculations({Speed, Value}, Fuel),
    S#state{
        fuel = Fuel - Burnt,
        speed = Speed + Acceleration,
        distance = Distance + Travelled,
        burnt = B + Burnt
    };
next_state(S, _V, {call, _, brake, [Value]}) ->
    #state{
        fuel = Fuel,
        speed = Speed,
        distance = Distance,
        burnt = B
    } = S,
    {Travelled, Deceleration, _, Burnt} = acceleration_calculations({Speed, -Value}, Fuel),
    S#state{
        fuel = Fuel - Burnt,
        speed = Speed + Deceleration,
        distance = Distance + Travelled,
        burnt = B + Burnt
    };
next_state(S, _V, {call, _, travel, [Value]}) ->
    #state{
        fuel = Fuel,
        speed = Speed,
        distance = Distance,
        burnt = B
    } = S,
    {Travelled, _, Burnt} = travel_calculations(Value, Speed, Fuel),
    S#state{
        fuel = Fuel - Burnt,
        distance = Distance + Travelled,
        burnt = B + Burnt
    };
next_state(S, _V, {call, _, refuel, [Amount]}) ->
    #state{
        fuel = Fuel,
        speed = Speed,
        distance = Distance,
        burnt = B
    } = S,
    {Travelled, _Deceleration, _, Burnt} = acceleration_calculations({Speed, -Speed}, Fuel),
    S#state{
        fuel = Fuel - Burnt + Amount,
        speed = 0,
        distance = Distance + Travelled,
        burnt = B + Burnt
    }.

%% ----------------------------------------------------------------------------
%% Properties
%% ----------------------------------------------------------------------------

prop_server_distance() ->
    ?FORALL(Cmds, proper_statem:weighted_commands(?MODULE, [1, 5, 10, 1]),
        ?TRAPEXIT(
            begin
                ?SERVER:start_link(),
                {_History, State, Result} = run_commands(?MODULE, Cmds),
                ?SERVER:stop(),
                #state{distance = Distance, burnt = Burnt} = State,
                ?WHENFAIL(io:format("Distance: ~p~nConsumption: ~p~n",
                                    [Distance, 100 * Burnt / Distance]),
                          aggregate(command_names(Cmds), Result =:= ok andalso (Distance < 1000 orelse 100 * Burnt / Distance > 7)))
            end)).

prop_server_targeted() ->
    ?FORALL_TARGETED(
        Mixed,
        proper_statem:targeted_commands(?MODULE),
        ?TRAPEXIT(
            begin
                {_, Cmds, _} = Mixed,
                case length(Cmds) > 0 of
                    true ->       
                        ?SERVER:start_link(),
                        {_History, State, Result} = run_commands(?MODULE, Cmds),
                        ?SERVER:stop(),
                        #state{distance = Distance, burnt = Burnt} = State,
                        Consumption = 100 * Burnt / Distance,
                        UV = case Consumption > 10 of
                            true -> 0;
                            false -> Distance
                        end,
                        ?MAXIMIZE(UV),
                        ?WHENFAIL(
                            io:format("Distance: ~p~nConsumption: ~p~n", [Distance, Consumption]),
                            aggregate(
                                command_names(Cmds),
                                Result =:= ok andalso (Distance < 1000 orelse Consumption > 10)
                            )
                        );
                    false ->
                        true
                    end
            end
        )
    ).
