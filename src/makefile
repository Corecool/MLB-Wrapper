.SUFFIXES: .erl .beam

OUTPUTBEAM = ${MLB_WRAPPER_HOME}/ebin

.erl.beam:
	erlc -W $< ; mv $@ ${OUTPUTBEAM}

ERL = erl -boot start_clean 

MODS = lirs

all: compile

compile: ${MODS:%=%.beam}

clean:
	rm -fr erl_crash.dump
	cd ${OUTPUTBEAM} ; rm -rf *.beam 