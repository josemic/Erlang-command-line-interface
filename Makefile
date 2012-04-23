.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = sr_telnet sr_telnet_registration sr_parser sr_command sr_demo sr_gsmnet

all: compile
	${ERL} -pa '~/osmocom/telnetclient' -s sr_telnet server 1025
compile: ${MODS:%=%.beam}

clean: 
	rm -rf *.beam erl_crash.dump

