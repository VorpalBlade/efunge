%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008 Arvid Norlander <anmaster AT tele2 DOT se>
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%----------------------------------------------------------------------
%%% This files defines generic types used for OTP behaviour -specs in efunge.
-type chl_restart() :: permanent | transient | temporary.
-type chl_shutdown() :: brutal_kill | non_neg_integer() | infinity.
-type chl_type() :: worker | supervisor.
-type chl_modules() :: [atom()] | dynamic.
-type child_spec() :: {any(),mfa(),chl_restart(),chl_shutdown(),chl_type(),chl_modules()}.
-type child_specs() :: [child_spec(),...].

-type sup_restart_strategy() :: one_for_all | one_for_one | rest_for_one | simple_one_for_one.
-type supervisor_spec() :: {sup_restart_strategy(),non_neg_integer(),non_neg_integer()}.

-type otp_start_error() :: {error,{already_started, pid()} | any()}.
-type otp_start_return() :: {ok,pid()} | ignore | otp_start_error().

-type supervisor_return() :: {ok,{supervisor_spec(),child_specs()}} | ignore.

