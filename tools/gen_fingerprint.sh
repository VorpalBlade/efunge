#!/usr/bin/env bash
# -*- coding: utf-8 -*-
###########################################################################
#                                                                         #
#  efunge - a Befunge-98 interpreter in Erlang.                           #
#  Copyright (C) 2008-2009  Arvid Norlander                               #
#                                                                         #
#  This program is free software: you can redistribute it and/or modify   #
#  it under the terms of the GNU General Public License as published by   #
#  the Free Software Foundation, either version 3 of the License, or      #
#  (at the proxy's option) any later version. Arvid Norlander is a        #
#  proxy who can decide which future versions of the GNU General Public   #
#  License can be used.                                                   #
#                                                                         #
#  This program is distributed in the hope that it will be useful,        #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of         #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          #
#  GNU General Public License for more details.                           #
#                                                                         #
#  You should have received a copy of the GNU General Public License      #
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.  #
#                                                                         #
###########################################################################

# Generate a fingerprint template.
# This must be run from top source directory.

# Error to fail with for old bash.
fail_old_bash() {
	echo "Sorry your bash version is too old!"
	echo "You need at least version 3.2.10 of bash"
	echo "Please install a newer version:"
	echo " * Either use your distro's packages"
	echo " * Or see http://www.gnu.org/software/bash/"
	exit 2
}

# Check bash version. We need at least 3.2.10
# Lets not use anything like =~ here because
# that may not work on old bash versions.
if [[ "${BASH_VERSINFO[0]}${BASH_VERSINFO[1]}" -lt 32 ]]; then
	fail_old_bash
elif [[ "${BASH_VERSINFO[0]}${BASH_VERSINFO[1]}" -eq 32 && "${BASH_VERSINFO[2]}" -lt 10 ]]; then
	fail_old_bash
fi

if [[ ! -d src/fingerprints ]]; then
	echo "ERROR: Run from top source directory please." >&2
	exit 1
fi

set -e

if [[ ! -f tools/fprint_funcs.sh ]]; then
	echo "ERROR: Couldn't find tools/fprint_funcs.sh." >&2
	exit 1
fi
source tools/fprint_funcs.sh
if [[ $? -ne 0 ]]; then
	echo "ERROR: Couldn't load tools/fprint_funcs.sh." >&2
	exit 1
fi

# Variables
FPRINT=""
FPRINTLOW=""
fp_URL=""
fp_SAFE=""
fp_CONDITION=""
fp_OPCODES=""
fp_DESCRIPTION=""

fp_OPCODES=""
fp_OPCODE_NAMES=()
fp_OPCODE_DESC=()

if [[ -z $1 ]]; then
	echo "ERROR: Please provide finger print name!" >&2
	echo "Usage: $0 FingerprintName opcodes" >&2
	exit 1
else
	FPRINT="$1"
fi

progress "Sanity checking parameters"
checkfprint "$FPRINT"

if [[ -e src/fingerprints/fing${FPRINT}.erl ]]; then
	die "A fingerprint with that name already exists"
fi

progress "Looking for spec file"

if [[ -f "src/fingerprints/${FPRINT}.spec" ]]; then
	status "Good, spec file found."
else
	die "Sorry you need a spec file for the fingerprint. It should be placed at src/fingerprints/${FPRINT}.spec"
fi

cd "src/fingerprints" || die "Couldn't change directory to src/fingerprints"

progress "Parsing spec file"
parse_spec "${FPRINT}"

# Check for unsupported features.
if [[ $fp_CONDITION ]]; then
	die "Sorry, efunge doesn't support %condition"
fi

addtoerl() {
	printf '%s\n' "$1" >> "fing${FPRINT}.erl"
}

addtoerl_nolf() {
	printf '%s' "$1" >> "fing${FPRINT}.erl"
}

FPRINTLOW="$(tr 'A-Z' 'a-z' <<< "$FPRINT")"


progress "Creating file"
cat > "fing${FPRINT}.erl" << EOF
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
EOF

addtoerl "%% @doc ${FPRINT} fingerprint."
addtoerl "-module(fing${FPRINT})."

cat >> "fing${FPRINT}.erl" << EOF
-export([load/1]).
%% The implemented functions
-export([
EOF

# Generate exports list.
for (( i = 0; i < ${#fp_OPCODES}; i++ )); do
	ord number "${fp_OPCODES:$i:1}"
	addtoerl_nolf "         ${FPRINTLOW}_${fp_OPCODE_NAMES[$number]}/3"
	if (( $i < ${#fp_OPCODES} - 1 )); then
		addtoerl ","
	fi
done
addtoerl "])."

cat >> "fing${FPRINT}.erl" << EOF

-include("../efunge_ip.hrl").
-include("../funge_types.hrl").

%% Import common functions:
-import(efunge_stackstack, [push/2, pop/1]).


EOF

addtoerl "%% @doc Load the $FPRINT fingerprint."
addtoerl "-spec load(ip()) -> {ok, ip()}."
addtoerl "load(IP) ->"
addtoerl "	IP2 = efunge_fingermanager:push_funs(IP, ["
for (( i = 0; i < ${#fp_OPCODES}; i++ )); do
	ord number "${fp_OPCODES:$i:1}"
	addtoerl_nolf "		{\$${fp_OPCODES:$i:1}, fun ?MODULE:${FPRINTLOW}_${fp_OPCODE_NAMES[$number]}/3}"
	if (( $i < ${#fp_OPCODES} - 1 )); then
		addtoerl ","
	fi
done
addtoerl "]),"
addtoerl '	{ok, IP2}.'
addtoerl ''
addtoerl ''
addtoerl '%% The fingerprint functions'
addtoerl ''

for (( i = 0; i < ${#fp_OPCODES}; i++ )); do
	ord number "${fp_OPCODES:$i:1}"
	funname="${FPRINTLOW}_${fp_OPCODE_NAMES[$number]}"

	addtoerl "%% @spec ${funname}(ip(), stackstack(), fungespace()) -> execute_return()"
	addtoerl "%% @doc ${fp_OPCODES:$i:1} - ${fp_OPCODE_DESC[$number]}"
	addtoerl "-spec ${funname}(ip(), stackstack(), fungespace()) -> execute_return()."
	addtoerl "${funname}(IP, Stack, Space) ->"
	addtoerl "	{efunge_ip:rev_delta(IP), Stack}."
	addtoerl ''
done


cat >> "fing${FPRINT}.erl" << EOF

%% Private funtions
EOF

status "File creation done"
echo
echo "To make efunge aware of the new fingerprint run tools/gen_fprint_list.sh"
echo
echo "All done! However make sure the copyright in the files is correct. Oh, and another thing: implement the fingerprint :)"
