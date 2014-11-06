%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008-2010 Arvid Norlander <anmaster AT tele2 DOT se>
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
-type child_specs() :: [supervisor:child_spec()].

-type supervisor_spec() :: {supervisor:stratergy(),non_neg_integer(),non_neg_integer()}.

-type supervisor_child_pid() :: pid() | undefined.
-type supervisor_child_error_basic() :: {already_started, supervisor_child_pid()}
                                      | any().
-type supervisor_child_error() :: already_present
                                | supervisor_child_error_basic().
-type supervisor_child_ok() :: {ok, supervisor_child_pid()}
                             | {ok, supervisor_child_pid(), _}.
-type supervisor_start_child_result() :: supervisor_child_ok()
                                       | {error, supervisor_child_error()}.


-type otp_start_error()            :: {error,{already_started, pid()} | any()}.
-type otp_start_return_no_ignore() :: {ok,pid()} | otp_start_error().
-type otp_start_return()           :: otp_start_return_no_ignore() | ignore.

-type supervisor_return_no_ignore() :: {ok,{supervisor_spec(),child_specs()}}.
-type supervisor_return() :: supervisor_return_no_ignore() | ignore.

-type app_start_type() :: normal | {takeover, atom()} | {failover, atom()}.

