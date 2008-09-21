ERL = erl -boot start_clean
RM = rm -rf
all:
	$(ERL) -make
clean:
	$(RM) ebin/*.beam *~ */*~ */*/*~ erl_crash.dump
