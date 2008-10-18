{application, efunge,
 [
  {description, "A Befunge-98 interpreter in Erlang."},
  {vsn, "0.0.1"},
  {applications, [kernel, stdlib]},
  {modules, [efunge,
             efunge_app,
             efunge_fingerindex,
             efunge_fingermanager,
             efunge_fingerstack,
             efunge_fungespace,
             efunge_input,
             efunge_interpreter,
             efunge_ip,
             efunge_stack,
             efunge_stackstack,
             efunge_supervisor_services,
             efunge_supervisor_top,
             efunge_sysinfo,
             fingCPLI,
             fingFIXP,
             fingMODU,
             fingNULL,
             fingROMA
            ]},
  {registered, [efunge_supervisor_top,
                efunge_supervisor_services,
                efunge_input
               ]},
  {mod, {efunge_app,[]}}
 ]
}.
