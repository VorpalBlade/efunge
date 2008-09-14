ERL = erl -boot start_clean
all:
	$(ERL) -noshell -eval "make:all(), init:stop()."
clean:
	rm -rf *.beam *~ erl_crash.dump
