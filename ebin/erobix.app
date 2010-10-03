{application, erobix,
 [{description, "Erlang oBIX Server"},
  {vsn, "0.1"},
  {modules, [
    erobix,
    erobix_app,
    erobix_sup,
    erobix_web,
    erobix_deps,
    erobix_lib,
    erobix_router,
    erobix_lobby
  ]},
  {registered, []},
  {mod, {erobix_app, []}},
  {env, [
        ]},
  {applications, [kernel, stdlib, crypto, ssl, log4erl, erldis]}]}.
