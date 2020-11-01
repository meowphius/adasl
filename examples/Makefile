
ifeq (gnat.adc,$(wildcard gnat.adc))
  KRUNCH=y
endif

ifdef KRUNCH
  ADAFLAGS = -A.. -g -gnatk8
  KRPROG = gnatkr
  GEN =
else
  ADAFLAGS = -A.. -g
  KRPROG = echo
  GEN = gen
endif


EXAMPLES = state_path demo_graph1 count_words
SOCKET_BASED_EXAMPLES = telnet_server telnet_cmdproc

C_S = $(patsubst %,b_%.c,$(EXAMPLES))

all : examples

examples : 
	for i in $(EXAMPLES); do \
		gnatmake $(ADAFLAGS) `$(KRPROG) $$i` ;\
	done
	for i in $(SOCKET_BASED_EXAMPLES); do \
		gnatmake $(ADAFLAGS) `$(KRPROG) $$i` `adasockets-config`;\
	done

krunch :
	cat *.ad[sb] >t1
	rm *.ad[sb]
	gnatchop -k t1
	rm t1

clean::
	rm -f *.o *.ali $(EXAMPLES) $(C_S) $(GENNED_FILES) b~* \
		$(SOCKET_BASED_EXAMPLES)
