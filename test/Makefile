
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

TESTS = test_tree \
	test_tree_balanced \
	test_tree_balanced_random \
	test_list \
	test_ordered \
	test_ordered_sort \
	test_hash \
	test_hash_random \
	test_heap \
	test_heap_random \
	test_sets \
	test_graph \
	test_digraph \
	test_semaphore \
	test_btree \
	test_btree_random \
	test_telnet \
	test_string_tokenizer \
	test_abstract_io \
	test_refcount_ptr \
	test_calendar

C_S = $(patsubst %,b_%.c,$(TESTS))

all : tests

tests : 
	for i in $(TESTS); do \
		gnatmake $(ADAFLAGS) `$(KRPROG) $$i` || exit 1;\
	done
	for i in $(TESTS); do \
		echo -n "Running $$i - "; \
		./`$(KRPROG) $$i` || exit 1; \
	done

krunch :
# Move this first because gnatchop has problems with the task
# dispatching pragma.
	mv test_semaphore.adb t1
	cat *.ad[sb] >>t1
	rm *.ad[sb]
	gnatchop -k t1
	rm t1

clean::
	rm -f *.o *.ali $(TESTS) $(C_S) $(GENNED_FILES) b~*
