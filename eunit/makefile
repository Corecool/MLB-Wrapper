.SUFFIXES: .erl .beam

OUTPUTBEAM = ${PWD}/ebin

.erl.beam:
	erlc -D DEBUG -o ${OUTPUTBEAM} -W $<

ERL = erl -boot start_clean -pa ${MLB_WRAPPER_HOME}/ebin

MODS = rm_tests lirs_tests 

all: compile
	${ERL} -pa ${OUTPUTBEAM} -s rm_tests test \
				 -s lirs_tests test \
				 -s init stop

compile: ${MODS:%=%.beam}

clean:
	rm -fr erl_crash.dump
	cd ${OUTPUTBEAM} ; rm -rf *.beam 