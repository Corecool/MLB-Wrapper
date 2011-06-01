.SUFFIXES: .erl .beam

OUTPUTBEAM = ${MLB_WRAPPER_HOME}/ebin

.erl.beam:
	erlc -o ${OUTPUTBEAM} -W $<

ERL = erl -boot start_clean 

MODS = rm lirs

all: compile

compile: ${MODS:%=%.beam}

clean:
	rm -fr erl_crash.dump
	cd ${OUTPUTBEAM} ; rm -rf *.beam 