.SUFFIXES: .erl .beam

OUTPUTBEAM = ${PWD}/ebin

.erl.beam:
	erlc -D DEBUG -o ${OUTPUTBEAM} -W $<

ERL = erl -boot start_clean -pa ${MLB_WRAPPER_HOME}/ebin \
	-sname node5 # -remsh 'node1@corecool-laptop'

MODS = remote_rm_tests rm_tests lirs_tests cache_tests \
       client_tests monitor_tests

all: compile
	${ERL} -pa ${OUTPUTBEAM} -s remote_rm_tests test \
				 -s rm_tests test \
				 -s lirs_tests test \
				 -s cache_tests test \
				 -s client_tests test \
				 -s monitor_tests test \
				 -s init stop

# all: compile
# 	${ERL} -pa ${OUTPUTBEAM} -s client_tests test \
# 				 -s init stop

compile: srcdir ${MODS:%=%.beam} 

srcdir:
	cd ${MLB_WRAPPER_HOME}/src; make -k

clean:
	rm -fr erl_crash.dump
	cd ${OUTPUTBEAM} ; rm -rf *.beam 