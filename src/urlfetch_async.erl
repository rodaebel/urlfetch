-module(urlfetch_async).

-export([fetch/1, fetch/7, process_record/1, get_result/1, flush/1]).

-include("urlfetch.hrl").


fetch({Id, Method, Url, Payload, Headers}) ->
    Record = #cache{id=Id, timestamp=urlfetch_cache:timestamp()},
    process_record(Record),
    spawn(urlfetch_async, fetch,
          [Id, Url, Method, Payload, Headers, ?RETRY_COUNT, ?RETRY_TIMEOUT]),
    ok.

 
fetch(Id, Url, Method, Payload, Headers, Retry, Sleep) when Retry > 0 ->
    case Method of
        get ->
            Request = {Url, Headers};
        head ->
            Request = {Url, Headers};
        post ->
            Request = {Url, Headers, "", Payload}
    end,
    case httpc:request(Method, Request, [], [{sync, false}, {stream, self}]) of
        {ok, ReqId} ->
            case receive_chunk(Id, ReqId) of
                {ok, _} ->
                    ok;
                {error, unauthorized, {_Status, _Headers}} ->
                    ok;
                {error, timeout} ->
                    error_logger:info_msg("~p Timeout.~n", [self()]),
                    timer:sleep(Sleep),
                    fetch(Id, Url, Method, Payload, Headers, Retry-1, Sleep);
                {_, Reason} ->
                    error_logger:error_msg("~p~n", [Reason]),
                    timer:sleep(Sleep),
                    fetch(Id, Url, Method, Payload, Headers, Retry-1, Sleep)
            end;
        _ ->
            timer:sleep(Sleep),
            fetch(Id, Url, Method, Payload, Headers, Retry-1, Sleep)
    end;
fetch(Id, _, _, _, _, Retry, _) when Retry =< 0 ->
    error_logger:info_msg("~p Giving up on ~p.~n", [self(), Id]),
    flush(Id),
    {error, no_more_retry}.


process_record(Record) ->
    urlfetch_cache ! {store, Record, self()},
    receive
        true ->
            ok;
        false ->
            error
    end.

 
receive_chunk(Id, ReqId) ->
    receive
        {http, {ReqId, {error, Reason}}} when(Reason =:= etimedout) orelse(Reason =:= timeout) -> 
            {error, timeout};
        {http, {ReqId, {{_, 401, _} = Status, Headers, _}}} -> 
            Record = #cache{
                id=Id, status_code=401, complete=true,
                timestamp=urlfetch_cache:timestamp()},
            process_record(Record),
            {error, unauthorized, {Status, Headers}};
        {http, {ReqId, Result}} -> 
            {error, Result};
        {http, {ReqId, stream_start, Headers}} ->
            EncodedHeaders = list_to_binary(
                urlfetch_http:encode_headers(Headers) ++ "\n\n"),
            Record = #cache{
                id=Id, data=EncodedHeaders,
                timestamp=urlfetch_cache:timestamp()},
            process_record(Record),
            receive_chunk(Id, ReqId);
        {http, {ReqId, stream, Data}} ->
            Record = #cache{
                id=Id, data=Data, timestamp=urlfetch_cache:timestamp()},
            process_record(Record),
            receive_chunk(Id, ReqId);
        {http, {ReqId, stream_end, _Headers}} ->
            Record = #cache{
                id=Id, complete=true, timestamp=urlfetch_cache:timestamp()},
            process_record(Record),
            {ok, ReqId}
 
        after 10 * 1000 ->
            {error, timeout}
    end.


get_result(Id) ->
    urlfetch_cache ! {fetch, Id, self()},
    receive
        {ok, Data} ->
            {result, Data};
        retry ->
            {error, retry};
        not_found ->
            {error, not_found}
    end.


flush(Id) ->
    urlfetch_cache ! {delete, Id, self()},
    ok.
