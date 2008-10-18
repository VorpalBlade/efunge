{application, efunge,
 [
  {description, "A Befunge-98 interpreter in Erlang."},
  {vsn, "0.0.1"},
  {applications, [kernel, stdlib]},
  {registered, [efunge_supervisor_top,
                efunge_supervisor_services,
                efunge_input
               ]},
  {mod, {efunge_app,[]}}
 ]
}.
