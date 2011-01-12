{application, erobix,
 [{description, "eroBIX - Erlang oBIX Server"},
  {vsn, "0.1"},
  {modules, [
    erobix,
    erobix_app,
    erobix_sup,
    erobix_web,
    erobix_deps,
    erobix_lib,
    erobix_router,
    erobix_lobby,
    erobix_about,
    erobix_redis_store,
    erobix_object_server,
    erobix_data_sup,
    erobix_data_server
  ]},
  {registered, []},
  {mod, {erobix_app, []}},
  {env, [
         {store, erobix_redis_store}
        ]},
  {applications, [kernel, stdlib, crypto, public_key, ssl, log4erl, erldis]}]}.
