ERL = erl -boot start_clean
RM = rm -rf
all:
	$(ERL) -noshell -eval "make:all(), init:stop()."
clean:
	$(RM) ebin/*.beam *~ */*~ */*/*~ erl_crash.dump
