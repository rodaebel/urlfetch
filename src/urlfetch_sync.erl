-module(urlfetch_sync).

-export([fetch/1]).


fetch({Method, Url}) ->
    {ok, {_Status, Head, Body}} = httpc:request(Method, {Url, []}, [], []),
    Length = list_to_binary(proplists:get_value("content-length", Head, "0")),
    {ok, Length, Body}.
