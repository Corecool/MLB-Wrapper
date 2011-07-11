.SUFFIXES: .erl .beam

OUTPUTBEAM = ${MLB_WRAPPER_HOME}/ebin

.erl.beam:
	erlc +debug_info -DTEST -o ${OUTPUTBEAM} -W $<

ERL = erl -boot start_clean 

MODS = remote_rm rm lirs cache client monitor

all: check

compile: ${MODS:%=%.beam}

check: compile
	dialyzer --plt ${MLB_WRAPPER_HOME}/mlb.plt \
	-r ${OUTPUTBEAM}

clean:
	rm -fr erl_crash.dump
	cd ${OUTPUTBEAM} ; rm -rf *.beam 