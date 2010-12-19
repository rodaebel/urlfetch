{application, urlfetch_service,
 [
  {description, "URL Fetch Service"},
  {vsn, "1.0"},
  {id, "urlfetch_service"},
  {modules,      [urlfetch_listener, urlfetch_handler, urlfetch_rpc_parser,
                  urlfetch_async, urlfetch_cache, urlfetch_http,
                  urlfetch_uuid]},
  {registered,   [urlfetch_service_sup, urlfetch_listener]},
  {applications, [kernel, stdlib]},
  {mod, {urlfetch_service_app, []}},
  {env, []}
 ]
}.
