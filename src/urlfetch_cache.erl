-module(urlfetch_cache).

-export([start/0, loop/0, timestamp/0]).

-include_lib("urlfetch.hrl").


%% External API

start() ->
    error_logger:info_msg("~p Starting cache server.~n", [self()]),

    %% Create tables
    ets:new(cache_table, [public, named_table, ordered_set]),

    %% Register node
    register(?MODULE, spawn(?MODULE, loop, [])).


%% @spec loop() -> null
%% @doc  Recursion loop for our cache server.
loop() ->
    receive
        {store, Record, Client} ->
            case store(Record) of
                true ->
                    Client ! true;
                false ->
                    Client ! false
            end;
        {exists, Id, Client} ->
            case ets:member(cache_table, Id) of
                true ->
                    Client ! true;
                false ->
                    Client ! false
            end;
        {delete, Id, Client} ->
            case delete(Id) of
                true ->
                    Client ! true;
                false ->
                    Client ! false
            end;
        {fetch, Id, Client} ->
            case fetch(Id) of
                {ok, Data} ->
                    Client ! {ok, Data};
                not_found ->
                    Client ! not_found;
                retry -> Client ! retry
            end
    after 60000 ->
        flush_expired()
    end,
    loop().


%% Internal API

store(Record) ->
    Id        = Record#cache.id,
    Status    = Record#cache.status_code,
    Data      = Record#cache.data,
    Complete  = Record#cache.complete,
    Timestamp = Record#cache.timestamp,

    error_logger:info_msg("~p Inserting data for ~p.~n", [self(), Id]),

    case ets:match(cache_table, {Id, '$1', '$2', false, '$3'}) of
        [] ->
            ets:insert(cache_table, {Id, Status, Data, false, Timestamp});
        [[_, OldData, _]] ->
            ets:insert(
                cache_table,
                {Id, Status, <<OldData/binary, Data/binary>>, Complete, Timestamp})
    end.


delete(Id) ->
    error_logger:info_msg("~p Deleting data for ~p.~n", [self(), Id]),
    case ets:lookup(cache_table, Id) of
        [] ->
            false;
        [{_, Status, Data, Complete, Timestamp}] ->
            ets:delete_object(cache_table,
                              {Id, Status, Data, Complete, Timestamp})
    end.


fetch(Id) ->
    %% error_logger:info_msg("~p Looking up data for ~p.~n", [self(), Id]),
    case ets:lookup(cache_table, Id) of
        [] ->
            not_found;
        [{_, Status, Data, Complete, _}] ->
            case Complete of
                true ->
                    {ok, {Status, Data}};
                false ->
                    retry
            end
    end.


timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:now(),
    MegaSecs * 1000000 + Secs.


flush_expired() ->
    T = timestamp() - ?EXPIRATION_INTERVAL,
    S = [{{'$1', '$2', '$3', '$4', '$5'}, [{'<', '$5', {const, T}}], [true]}],
    N = ets:select_delete(cache_table, S),
    if N > 0 ->
        error_logger:info_msg(
            "~p ~p expired record(s) flushed.~n", [self(), N]);
        true ->
            false
    end.
