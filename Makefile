EFLAGS ?=
.SUFFIXES: .erl .beam
.erl.beam:
	erlc -W $(EFLAGS) $<
ERL = erl -boot start_clean
MODS = efunge fspace fstack
all: compile
compile: ${MODS:%=%.beam}
clean:
	rm -rf *.beam *~ erl_crash.dump
