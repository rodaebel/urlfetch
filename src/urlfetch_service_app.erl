-module(urlfetch_service_app).

-behaviour(application). %% Implements application API.

-include("urlfetch.hrl").

-export([start_client/0]).

-export([start/2, stop/1, init/1]). %% Application and Supervisor callbacks.


%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client() ->
    supervisor:start_child(urlfetch_client_sup, []).


start(_Type, _Args) ->
    urlfetch_cache:start(),
    inets:start(),
    ssl:start(),
    ListenPort = get_app_env(listen_port, ?PORT),
    supervisor:start_link({local, ?MODULE}, ?MODULE,
                          [ListenPort, urlfetch_handler]).


stop(_S) ->
    ok.


init([Port, Module]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP listener
              {   urlfetch_service_sup,
                  {urlfetch_listener, start_link, [Port, Module]},
                  permanent,
                  2000,
                  worker,
                  [urlfetch_listener]
              },
              % UUID generator
              {   urlfetch_uuid_sup,
                  {urlfetch_uuid, start, []},
                  permanent,
                  2000,
                  worker,
                  [urlfetch_uuid]
              },
              % Client instance supervisor
              {   urlfetch_client_sup,
                  {supervisor, start_link, [{local, urlfetch_client_sup},
                                            ?MODULE, [Module]]},
                  permanent,
                  infinity,
                  supervisor,
                  []
              }
            ]
        }
    };
init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,
                  {Module, start_link, []},
                  temporary,
                  2000,
                  worker,
                  []
              }
            ]
        }
    }.


get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.
