EFLAGS ?= +warn_obsolete_guard +warn_untyped_record +warn_unused_import +warn_missing_spec

.SUFFIXES: .erl .beam
.erl.beam:
	erlc -W $(EFLAGS) $<
ERL = erl -boot start_clean
MODS = efunge fspace fstack
all: compile
compile: ${MODS:%=%.beam}
clean:
	rm -rf *.beam *~ erl_crash.dump
