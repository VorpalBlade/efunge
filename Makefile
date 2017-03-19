ERL = erl -boot start_clean
RM = rm -rf
all: ebin
	$(ERL) -make
clean:
	$(RM) ebin/*.beam *~ */*~ */*/*~ erl_crash.dump
ebin:
	mkdir ebin
