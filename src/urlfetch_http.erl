-module(urlfetch_http).

-include_lib("eunit/include/eunit.hrl").

-export([decode_headers/1]).


decode_headers(String) ->
  decode_headers(String, [], []).

decode_headers([], [], Result) ->
  lists:reverse(Result);
decode_headers([], Buffer, [{Key}|Result]) ->
  decode_headers([], [], [{Key, lists:reverse(Buffer)}|Result]);
decode_headers(": " ++ String, Buffer, Result) ->
  decode_headers(String, [], [{lists:reverse(Buffer)}|Result]);
decode_headers("\n" ++ String, Buffer, [{Key}|Result]) ->
  decode_headers(String, [], [{Key, lists:reverse(Buffer)}|Result]);
decode_headers([C|String], Buffer, Result) ->
  decode_headers(String, [C|Buffer], Result).


%% Unit tests.
decode_headers_test() ->
    ?assert(decode_headers("Content-Type: text/plain") =:= [{"Content-Type","text/plain"}]),
    ?assert(decode_headers("Content-Type: text/plain\nX-Custom-Header: foobar (1)") =:= [{"Content-Type","text/plain"},{"X-Custom-Header","foobar (1)"}]).
