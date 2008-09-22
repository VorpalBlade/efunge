#!/usr/bin/env bash
# -*- coding: utf-8 -*-
###########################################################################
#                                                                         #
#  cfunge - a conformant Befunge93/98/08 interpreter in C                 #
#  Copyright (C) 2008  Arvid Norlander                                    #
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

# Check bash version. We need at least 3.1
# Lets not use anything like =~ here because
# that may not work on old bash versions.
if [[ "${BASH_VERSINFO[0]}${BASH_VERSINFO[1]}" -lt 31 ]]; then
	echo "Sorry your bash version is too old!"
	echo "You need at least version 3.1 of bash."
	echo "Please install a newer version:"
	echo " * Either use your distro's packages."
	echo " * Or see http://www.gnu.org/software/bash/"
	exit 2
fi

set -e

# Variables
FPRINT=""
FPRINTLOW=""
URL=""
SAFE=""
OPCODES=""
DESCRIPTION=""

OPCODES=""
OPCODE_NAMES=()
OPCODE_DESC=()

# This must be run from top source directory.

die() {
	echo "ERROR: $1" >&2
	exit 1
}

progress() {
	echo " * ${1}..."
}
status() {
	echo "   ${1}"
}


# Char to decimal
ord() {
	printf -v "$1" '%d' "'$2"
}

if [[ -z $1 ]]; then
	echo "ERROR: Please provide finger print name!" >&2
	echo "Usage: $0 FingerprintName opcodes" >&2
	exit 1
else
	FPRINT="$1"
fi

progress "Sanity checking parameters"
if [[ $FPRINT =~ ^[A-Z0-9]{4}$ ]]; then
	status "Fingerprint name $FPRINT ok style."
# Yes those (space, / and \) break stuff...
# You got to create stuff on your own if you need those, and not include that
# in any function names or filenames.
elif [[ $FPRINT =~ ^[^\ /\\]{4}$ ]]; then
	status "Fingerprint name $FPRINT probably ok (but not common style)."
	status "Make sure each char is in the ASCII range 0-254."
	status "Note that alphanumeric (upper case only) fingerprint names are strongly prefered."
else
	die "Not valid format for fingerprint name."
fi

if [[ ! -d src/fingerprints ]]; then
	die "Run from top source directory please."
fi

if [[ -e src/fingerprints/fing${FPRINT}.erl ]]; then
	die "A fingerprint with that name already exists"
fi

progress "Looking for spec file"

if [[ -f "src/fingerprints/${FPRINT}.spec" ]]; then
	status "Good, spec file found."
else
	die "Sorry you need a spec file for the fingerprint. It should be placed at src/fingerprints/${FPRINT}.spec"
fi


progress "Parsing spec file"
IFS=$'\n'

# First line is %fingerprint-spec 1.2
exec 4<"src/fingerprints/${FPRINT}.spec"
read -ru 4 line
if [[ "$line" != "%fingerprint-spec 1.2" ]]; then
	die "Either the spec file is not a fingerprint spec, or it is not version 1.2 of the format."
fi

# 0: pre-"begin instrs"
# 1: "begin-instrs"
parsestate=0


while read -ru 4 line; do
	if [[ "$line" =~ ^# ]]; then
		continue
	fi
	if [[ $parsestate == 0 ]]; then
		IFS=':' read -rd $'\n' type data <<< "$line" || true
		case $type in
			"%fprint")
				if [[ "$FPRINT" != "$data" ]]; then
					die "fprint field doesn't match spec file name."
				fi
				;;
			"%url")
				URL="$data"
				;;
			"%f108-uri")
				# We don't need to care about this.
				;;
			"%alias")
				echo "Note: This script doesn't handle %alias, you got to add that on your own." >&2
				;;
			"%desc")
				DESCRIPTION="$data"
				;;
			"%safe")
				SAFE="$data"
				;;
			"%begin-instrs")
				parsestate=1
				;;
			"#"*)
				# A comment, ignore
				;;
			*)
				die "Unknown entry $type found."
				;;
		esac
	else
		if [[ "$line" == "%end" ]]; then
			break
		fi
		# Parse instruction lines.
		IFS=$'\t' read -rd $'\n' instr name desc <<< "$line"

		OPCODES+="$instr"
		ord number "${instr:0:1}"
		OPCODE_NAMES[$number]="$name"
		OPCODE_DESC[$number]="$desc"
	fi
done

unset IFS

status "Done parsing."

exec 4<&-

progress "Validating the parsed data"

if [[ "$URL" ]]; then
	status "%url: Good, not empty"
else
	die "%url is not given or is empty."
fi

if [[ "$DESCRIPTION" ]]; then
	status "%desc: Good, not empty"
else
	die "%desc is not given or is empty."
fi

if [[ ( "$SAFE" == "true" ) || ( "$SAFE" == "false" ) ]]; then
	status "%safe: OK"
else
	die "%safe must be either true or false."
fi

if [[ "$OPCODES" =~ ^[A-Z]+$ ]]; then
	# Check that they are sorted.
	previousnr=0
	for (( i = 0; i < ${#OPCODES}; i++ )); do
		ord number "${OPCODES:$i:1}"
		if [[ $previousnr -ge $number ]]; then
			die "Instructions not sorted or there are duplicates"
		else
			previousnr=$number
		fi
	done
	status "Instructions: OK"
else
	die "The opcodes are not valid. The must be in the range A-Z"
fi

FPRINTLOW="$(tr 'A-Z' 'a-z' <<< "$FPRINT")"


addtoerl() {
	printf '%s\n' "$1" >> "fing${FPRINT}.erl"
}

addtoerl_nolf() {
	printf '%s' "$1" >> "fing${FPRINT}.erl"
}



progress "Creating file"
cd "src/fingerprints/" || die "cd failed."
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
-include("../fip.hrl").
-include("../funge_types.hrl").
-export([load/1]).
%% The implemented functions
-export([
EOF

# Generate exports list.
for (( i = 0; i < ${#OPCODES}; i++ )); do
	ord number "${OPCODES:$i:1}"
	addtoerl_nolf "         ${FPRINTLOW}_${OPCODE_NAMES[$number]}/3"
	if (( $i < ${#OPCODES} - 1 )); then
		addtoerl ","
	fi
done
addtoerl "])."

cat >> "fing${FPRINT}.erl" << EOF

%% Import common functions:
-import(fstackstack, [push/2, pop/1]).


EOF

addtoerl "%% @doc Load the $FPRINT fingerprint."
addtoerl "-spec load(ip()) -> {ok, ip()}."
addtoerl "load(IP) ->"
addtoerl "	IP2 = ffingermanager:push_funs(IP, ["
for (( i = 0; i < ${#OPCODES}; i++ )); do
	ord number "${OPCODES:$i:1}"
	addtoerl_nolf "		{\$${OPCODES:$i:1}, fun ?MODULE:${FPRINTLOW}_${OPCODE_NAMES[$number]}/3}"
	if (( $i < ${#OPCODES} - 1 )); then
		addtoerl ","
	fi
done
addtoerl "]),"
addtoerl '	{ok, IP2}.'
addtoerl ''
addtoerl ''
addtoerl '%% The fingerprint functions'
addtoerl ''

for (( i = 0; i < ${#OPCODES}; i++ )); do
	ord number "${OPCODES:$i:1}"
	funname="${FPRINTLOW}_${OPCODE_NAMES[$number]}"

	addtoerl "%% @spec ${funname}(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}"
	addtoerl "%% @doc ${OPCODES:$i:1} - ${OPCODE_DESC[$number]}"
	addtoerl "-spec ${funname}(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}."
	addtoerl "${funname}(IP, Stack, Space) ->"
	addtoerl "	{fip:rev_delta(IP), Stack}."
	addtoerl ''
done


cat >> "fing${FPRINT}.erl" << EOF

%% Private funtions
EOF

status "File creation done"
echo
echo "To make cfunge aware of the new fingerprint run tools/gen_fprint_list.sh"
echo "You may need to run cmake or similar to make the build system aware as well."
echo
echo "All done! However make sure the copyright in the files is correct. Oh, and another thing: implement the fingerprint :)"
