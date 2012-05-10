.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = sr_telnet sr_telnet_registration sr_parser sr_command sr_config sr_demo sr_gsmnet sr_read_file

all: compile
	${ERL} -pa '~/osmocom/telnetclient' -s sr_telnet server 1025 "ConfigurationUnix.txt"
compile: ${MODS:%=%.beam}

clean: 
	rm -rf *.beam erl_crash.dump

