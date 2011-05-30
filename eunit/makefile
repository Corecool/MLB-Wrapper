.SUFFIXES: .erl .beam

OUTPUTBEAM = ${PWD}/ebin

.erl.beam:
	erlc -W $< ; mv $@ ${OUTPUTBEAM}

ERL = erl -boot start_clean -pa ${MLB_WRAPPER_HOME}/ebin

MODS = lirs_tests 

all: compile
	${ERL} -pa ${OUTPUTBEAM} -s lirs_tests test \
				 -s init stop

compile: ${MODS:%=%.beam}

clean:
	rm -fr erl_crash.dump
	cd ${OUTPUTBEAM} ; rm -rf *.beam 