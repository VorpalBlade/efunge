#!/usr/bin/env bash
# -*- coding: utf-8 -*-
###########################################################################
#                                                                         #
#  efunge - a Befunge-98 interpreter in Erlang.                           #
#  Copyright (C) 2008-2010  Arvid Norlander                               #
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

# Generate the fingerprint list
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

addtolist() {
	echo "$1" >> "../efunge_fingerindex.erl"
}


# This is to allow sorted list even with aliases...
# I hate aliases...
ENTRIES=()
ENTRYL1=()
ENTRYL2=()

# $1 = fprint name
genfprintinfo() {
	local FPRINT="$1"

	# Variables
	local fp_URL fp_SAFE fp_OPCODES fp_DESCRIPTION
	local fp_F109_URI fp_CONDITION
	local fp_ALIASES
	local fp_OPCODE_NAMES fp_OPCODE_DESC

	progress "Processing $FPRINT"

	checkfprint "$FPRINT"

	if [[ ! -e fing${FPRINT}.erl ]]; then
		die "A fingerprint called $FPRINT is not yet generated!"
	fi

	progresslvl2 "Looking for spec file"

	if [[ -f "${FPRINT}.spec" ]]; then
		statuslvl2 "Good, spec file found."
	else
		die "Sorry you need a spec file for the fingerprint. It should be placed at src/fingerprints/${FPRINT}.spec"
	fi
	parse_spec "${FPRINT}"

	# Check for unsupported features.
	if [[ $fp_CONDITION ]]; then
		die "Sorry, efunge doesn't support %condition"
	fi

	progresslvl2 "Generating data for list"
	local FPRINTHEX FPRINTERLANGHEX
	for (( i = 0; i < ${#FPRINT}; i++ )); do
		printf -v hex '%x' "'${FPRINT:$i:1}"
		FPRINTHEX+="$hex"
	done
	FPRINTERLANGHEX="16#$FPRINTHEX"
	FPRINTHEX="0x$FPRINTHEX"
	ENTRIES+=( "$FPRINTHEX" )
	ENTRYL1[$FPRINTHEX]="%% ${FPRINT} - ${fp_DESCRIPTION}"
	ENTRYL2[$FPRINTHEX]="lookup(${FPRINTERLANGHEX})  -> { \"${fp_OPCODES}\", fun fing${FPRINT}:load/1 };"
	statuslvl2 "Done"
	if [[ ${!fp_ALIASES[*]} ]]; then
		progresslvl2 "Generating aliases..."
		local myalias
		for myalias in "${fp_ALIASES[@]}"; do
			checkfprint "$myalias"
			local ALIASHEX ALIASERLANGHEX
			for (( i = 0; i < ${#myalias}; i++ )); do
				printf -v hex '%x' "'${myalias:$i:1}"
				ALIASHEX+="$hex"
			done
			ALIASERLANGHEX="16#$ALIASHEX"
			ALIASHEX="0x$ALIASHEX"

			ENTRIES+=("$ALIASHEX")
			ENTRYL1[$ALIASHEX]="%% ${myalias} - Alias for ${FPRINT} - ${fp_DESCRIPTION}"
			ENTRYL2[$ALIASHEX]="lookup(${ALIASERLANGHEX})  -> { \"${fp_OPCODES}\", fun fing${FPRINT}:load/1 };"
		done
	fi
}

cd "src/fingerprints/" || die "change directory failed"

progress "Finding fingerprint list"
SPECS=( *.spec )

FPRINTS=()

for spec in "${SPECS[@]}"; do
	FPRINTS+=( ${spec%.spec} )
done

progress "Creating list file"
cat > "../efunge_fingerindex.erl" << EOF
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
%% @private
%% @doc This module implements a lookup "table" for fingerprints.
%% WARNING: This file is auto generated by a script. Do not change manually.
-module(efunge_fingerindex).
-include("efunge_ip.hrl").
-include("funge_types.hrl").
-export([lookup/1]).

%% @type fingerloadingfun() = function((ip()) -> {ok, ip()} | {error, ip()}).
%%   A fingerprint loader function.
%% @type fingerstack() = [] | list(fingerfun()).
%%   Stack is a list, access at list head.

%% @spec lookup(integer()) -> {string(), fingerloadingfun()} | notfound
%% @doc Look up loader function and implemented instrs for a fingerprint.
%% If fingerprint isn't implemented the atom notfound will be returned.
-spec lookup(integer()) -> {string(), fingerloadingfun()} | notfound.
EOF

for fprint in "${FPRINTS[@]}"; do
	genfprintinfo "$fprint"
done
# I really hate aliases...
SORTEDENTRIES=( $(IFS=$'\n'; echo "${ENTRIES[*]}" | sort -n) )
for entry in "${SORTEDENTRIES[@]}"; do
	addtolist "${ENTRYL1[$entry]}"
	addtolist "${ENTRYL2[$entry]}"
done


cat >> "../efunge_fingerindex.erl" << EOF
lookup(_Fingerprint) -> notfound.
EOF

status "Done"
