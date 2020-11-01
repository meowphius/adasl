
VERSION = 1.4

ifeq (gnat.adc,$(wildcard gnat.adc))
  KRUNCH=y
endif

CFLAGS = -O2 -g

ifdef KRUNCH
  ADAFLAGS = -gnatk8 $(CFLAGS)
  KRPROG = gnatkr
  GEN =
else
  ADAFLAGS = $(CFLAGS)
  KRPROG = echo
  GEN = gen
endif

GENNED_FILES =	asgc-tree-dynamic.ads \
		asgc-tree-dynamic.adb \
		asgc-tree-fixed.ads \
		asgc-tree-fixed.adb \
		asgc-tree-expandable.ads \
		asgc-tree-expandable.adb \
		asgc-list-dynamic.ads \
		asgc-list-dynamic.adb \
		asgc-list-fixed.ads \
		asgc-list-fixed.adb \
		asgc-list-expandable.ads \
		asgc-list-expandable.adb \
		asgc-heap-dynamic.ads \
		asgc-heap-dynamic.adb \
		asgc-heap-fixed.ads \
		asgc-heap-fixed.adb \
		asgc-heap-expandable.ads \
		asgc-heap-expandable.adb \
		asgc-hash-dynamic.ads \
		asgc-hash-dynamic.adb \
		asgc-hash-fixed.ads \
		asgc-hash-fixed.adb \
		asgc-hash-expandable.ads \
		asgc-hash-expandable.adb \
		asgc-ordered-dlist-dynamic.ads \
		asgc-ordered-dlist-dynamic.adb \
		asgc-ordered-dlist-fixed.ads \
		asgc-ordered-dlist-fixed.adb \
		asgc-ordered-dlist-expandable.ads \
		asgc-ordered-dlist-expandable.adb \
		asgc-ordered-alist-fixed.ads \
		asgc-ordered-alist-fixed.adb \
		asgc-ordered-alist-expandable.ads \
		asgc-ordered-alist-expandable.adb \
		asgc-ordered-vector-fixed.ads \
		asgc-ordered-vector-fixed.adb \
		asgc-ordered-vector-expandable.ads \
		asgc-ordered-vector-expandable.adb \
		asgc-ordered-sortable-dlist.ads \
		asgc-ordered-sortable-alist.ads \
		asgc-ordered-sortable-vector.ads \
		asgc-ordered-sortable-dlist-dynamic.ads \
		asgc-ordered-sortable-dlist-dynamic.adb \
		asgc-ordered-sortable-dlist-fixed.ads \
		asgc-ordered-sortable-dlist-fixed.adb \
		asgc-ordered-sortable-dlist-expandable.ads \
		asgc-ordered-sortable-dlist-expandable.adb \
		asgc-ordered-sortable-alist-fixed.ads \
		asgc-ordered-sortable-alist-fixed.adb \
		asgc-ordered-sortable-alist-expandable.ads \
		asgc-ordered-sortable-alist-expandable.adb \
		asgc-ordered-sortable-vector-fixed.ads \
		asgc-ordered-sortable-vector-fixed.adb \
		asgc-ordered-sortable-vector-expandable.ads \
		asgc-ordered-sortable-vector-expandable.adb \
		asgc-graph-dynamic-graph.ads \
		asgc-graph-dynamic-graph.adb \
		asgc-graph-fixed-graph.ads \
		asgc-graph-fixed-graph.adb \
		asgc-graph-expandable-graph.ads \
		asgc-graph-expandable-graph.adb \
		asgc-graph-dynamic-digraph.ads \
		asgc-graph-dynamic-digraph.adb \
		asgc-graph-fixed-digraph.ads \
		asgc-graph-fixed-digraph.adb \
		asgc-graph-expandable-digraph.ads \
		asgc-graph-expandable-digraph.adb \
		asgc-tree-dynamic_managed.ads \
		asgc-tree-dynamic_managed.adb \
		asgc-tree-fixed_managed.ads \
		asgc-tree-fixed_managed.adb \
		asgc-tree-expandable_managed.ads \
		asgc-tree-expandable_managed.adb \
		asgc-list-dynamic_managed.ads \
		asgc-list-dynamic_managed.adb \
		asgc-list-fixed_managed.ads \
		asgc-list-fixed_managed.adb \
		asgc-list-expandable_managed.ads \
		asgc-list-expandable_managed.adb \
		asgc-heap-dynamic_managed.ads \
		asgc-heap-dynamic_managed.adb \
		asgc-heap-fixed_managed.ads \
		asgc-heap-fixed_managed.adb \
		asgc-heap-expandable_managed.ads \
		asgc-heap-expandable_managed.adb \
		asgc-hash-dynamic_managed.ads \
		asgc-hash-dynamic_managed.adb \
		asgc-hash-fixed_managed.ads \
		asgc-hash-fixed_managed.adb \
		asgc-hash-expandable_managed.ads \
		asgc-hash-expandable_managed.adb \
		asgc-ordered-dlist-dynamic_managed.ads \
		asgc-ordered-dlist-dynamic_managed.adb \
		asgc-ordered-dlist-fixed_managed.ads \
		asgc-ordered-dlist-fixed_managed.adb \
		asgc-ordered-dlist-expandable_managed.ads \
		asgc-ordered-dlist-expandable_managed.adb \
		asgc-ordered-alist-fixed_managed.ads \
		asgc-ordered-alist-fixed_managed.adb \
		asgc-ordered-alist-expandable_managed.ads \
		asgc-ordered-alist-expandable_managed.adb \
		asgc-ordered-vector-fixed_managed.ads \
		asgc-ordered-vector-fixed_managed.adb \
		asgc-ordered-vector-expandable_managed.ads \
		asgc-ordered-vector-expandable_managed.adb \
		asgc-ordered-sortable-dlist-dynamic_managed.ads \
		asgc-ordered-sortable-dlist-dynamic_managed.adb \
		asgc-ordered-sortable-dlist-fixed_managed.ads \
		asgc-ordered-sortable-dlist-fixed_managed.adb \
		asgc-ordered-sortable-dlist-expandable_managed.ads \
		asgc-ordered-sortable-dlist-expandable_managed.adb \
		asgc-ordered-sortable-alist-fixed_managed.ads \
		asgc-ordered-sortable-alist-fixed_managed.adb \
		asgc-ordered-sortable-alist-expandable_managed.ads \
		asgc-ordered-sortable-alist-expandable_managed.adb \
		asgc-ordered-sortable-vector-fixed_managed.ads \
		asgc-ordered-sortable-vector-fixed_managed.adb \
		asgc-ordered-sortable-vector-expandable_managed.ads \
		asgc-ordered-sortable-vector-expandable_managed.adb \
		asgc-graph-dynamic-graph_managed.ads \
		asgc-graph-dynamic-graph_managed.adb \
		asgc-graph-fixed-graph_managed.ads \
		asgc-graph-fixed-graph_managed.adb \
		asgc-graph-expandable-graph_managed.ads \
		asgc-graph-expandable-graph_managed.adb \
		asgc-graph-dynamic-digraph_managed.ads \
		asgc-graph-dynamic-digraph_managed.adb \
		asgc-graph-fixed-digraph_managed.ads \
		asgc-graph-fixed-digraph_managed.adb \
		asgc-graph-expandable-digraph_managed.ads \
		asgc-graph-expandable-digraph_managed.adb \
		asgc-graph-links-dynamic.ads \
		asgc-graph-links-dynamic.adb \
		asgc-graph-links-expandable.ads \
		asgc-graph-links-expandable.adb \
		asgc-graph-links-fixed.ads \
		asgc-graph-links-fixed.adb \
		asgc-graph-links-dynamic_managed.ads \
		asgc-graph-links-dynamic_managed.adb \
		asgc-graph-links-expandable_managed.ads \
		asgc-graph-links-expandable_managed.adb \
		asgc-btree-dynamic.ads \
		asgc-btree-dynamic.adb \
		asgc-btree-dynamic_managed.ads \
		asgc-btree-dynamic_managed.adb \
		asl-date_time-register_simple_timezones.adb \
		asl-date_time-register_complex_timezones.adb

ASGC_SPECS = asgc-btree-dynamic.spec asgc-graph-links-dynamic.spec \
	asgc-graph-links-expandable.spec asgc-graph-links-fixed.spec \
	asgc-graph.spec asgc-hash-dynamic.spec asgc-hash-expandable.spec \
	asgc-hash-fixed.spec asgc-heap.spec asgc-list.spec \
	asgc-ordered-alist.spec asgc-ordered-dlist.spec \
	asgc-ordered-vector.spec asgc-tree.spec

ASGC_BODYS = asgc-btree-dynamic.body asgc-graph-links-dynamic.body \
	asgc-graph-links-expandable.body asgc-graph-links-fixed.body \
	asgc-graph.body asgc-hash-dynamic.body asgc-hash-expandable.body \
	asgc-hash-fixed.body asgc-heap.body asgc-list.body \
	asgc-ordered-alist.body asgc-ordered-dlist.body \
	asgc-ordered-vector.body asgc-tree.body

SED_SCRIPTS = digraph.sed dynamic.sed expandable.sed unmanaged.sed \
	unsortable.sed fixed.sed fixup.sed graph.sed managed.sed sortable.sed

AWK_SCRIPTS = filetr.awk krit.awk makeadc.awk unkrit.awk zonetab.awk

# Files used to generate the sources
GEN_SOURCE = $(AWK_SCRIPTS) $(SED_SCRIPTS) $(ASGC_SPECS) $(ASGC_BODYS)

CHOPPED_FILES = asl-*.ad? asl_compile.ads asoc_cmp_managed.ads \
	baseclass.ad? asoc_collectible.ad? asoc_cmp1.ads asoc_cmp2.ads \
	socket_compiler.ads

TOOL_FILES = conv_zones.adb conv_zones_data.adb conv_zones_data.ads

TIMEZONE_FILES = Timezones/asia \
                 Timezones/etcetera \
                 Timezones/northamerica \
                 Timezones/southamerica \
                 Timezones/africa \
                 Timezones/australasia \
                 Timezones/europe \
                 Timezones/pacificnew \
                 Timezones/systemv \
                 Timezones/antarctica \
	         Timezones/backward

OTHER_ZONE_FILES = Timezones/zone.tab

MISC_TO_BUILD = findzone.o

all :: $(GEN) $(MISC_TO_BUILD)
	gnatmake $(ADAFLAGS) `$(KRPROG) asoc_cmp1`
	gnatmake $(ADAFLAGS) `$(KRPROG) asoc_cmp2`
	gnatmake $(ADAFLAGS) `$(KRPROG) asoc_cmp_managed`
	gnatmake $(ADAFLAGS) `$(KRPROG) asl_compile`
	gnatmake $(ADAFLAGS) `$(KRPROG) socket_compile` `adasockets-config`

tests :: $(GEN)
	(cd tests; make )

examples :: $(GEN)
	(cd examples; make)

everything : all tests examples

gen : $(GENNED_FILES)

asgc-tree-dynamic.ads : asgc-tree.spec
	sed -f dynamic.sed $< | sed -f unmanaged.sed >$@

asgc-tree-dynamic.adb : asgc-tree.body
	sed -f dynamic.sed $< | sed -f unmanaged.sed >$@

asgc-tree-fixed.ads : asgc-tree.spec
	sed -f fixed.sed $< | sed -f unmanaged.sed >$@

asgc-tree-fixed.adb : asgc-tree.body
	sed -f fixed.sed $< | sed -f unmanaged.sed >$@

asgc-tree-expandable.ads : asgc-tree.spec
	sed -f expandable.sed $< | sed -f unmanaged.sed >$@

asgc-tree-expandable.adb : asgc-tree.body
	sed -f expandable.sed $< | sed -f unmanaged.sed >$@

asgc-list-dynamic.ads : asgc-list.spec
	sed -f dynamic.sed $< | sed -f unmanaged.sed >$@

asgc-list-dynamic.adb : asgc-list.body
	sed -f dynamic.sed $< | sed -f unmanaged.sed >$@

asgc-list-fixed.ads : asgc-list.spec
	sed -f fixed.sed $< | sed -f unmanaged.sed >$@

asgc-list-fixed.adb : asgc-list.body
	sed -f fixed.sed $< | sed -f unmanaged.sed >$@

asgc-list-expandable.ads : asgc-list.spec
	sed -f expandable.sed $< | sed -f unmanaged.sed >$@

asgc-list-expandable.adb : asgc-list.body
	sed -f expandable.sed $< | sed -f unmanaged.sed >$@

asgc-heap-dynamic.ads : asgc-heap.spec
	sed -f dynamic.sed $< | sed -f unmanaged.sed >$@

asgc-heap-dynamic.adb : asgc-heap.body
	sed -f dynamic.sed $< | sed -f unmanaged.sed >$@

asgc-heap-fixed.ads : asgc-heap.spec
	sed -f fixed.sed $< | sed -f unmanaged.sed >$@

asgc-heap-fixed.adb : asgc-heap.body
	sed -f fixed.sed $< | sed -f unmanaged.sed >$@

asgc-heap-expandable.ads : asgc-heap.spec
	sed -f expandable.sed $< | sed -f unmanaged.sed >$@

asgc-heap-expandable.adb : asgc-heap.body
	sed -f expandable.sed $< | sed -f unmanaged.sed >$@

asgc-btree-dynamic.ads : asgc-btree-dynamic.spec
	sed -f unmanaged.sed $< >$@

asgc-btree-dynamic.adb : asgc-btree-dynamic.body
	sed -f unmanaged.sed $< >$@

asgc-hash-dynamic.ads : asgc-hash-dynamic.spec
	sed -f unmanaged.sed $< >$@

asgc-hash-dynamic.adb : asgc-hash-dynamic.body
	sed -f unmanaged.sed $< >$@

asgc-hash-fixed.ads : asgc-hash-fixed.spec
	sed -f unmanaged.sed $< >$@

asgc-hash-fixed.adb : asgc-hash-fixed.body
	sed -f unmanaged.sed $< >$@

asgc-hash-expandable.ads : asgc-hash-expandable.spec
	sed -f unmanaged.sed $< >$@

asgc-hash-expandable.adb : asgc-hash-expandable.body
	sed -f unmanaged.sed $< >$@

asgc-ordered-dlist-dynamic.ads : asgc-ordered-dlist.spec
	sed -f dynamic.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@

asgc-ordered-dlist-dynamic.adb : asgc-ordered-dlist.body
	sed -f dynamic.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@

asgc-ordered-dlist-fixed.ads : asgc-ordered-dlist.spec
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@

asgc-ordered-dlist-fixed.adb : asgc-ordered-dlist.body
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@

asgc-ordered-dlist-expandable.ads : asgc-ordered-dlist.spec
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@

asgc-ordered-dlist-expandable.adb : asgc-ordered-dlist.body
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@

asgc-ordered-alist-fixed.ads : asgc-ordered-alist.spec
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@

asgc-ordered-alist-fixed.adb : asgc-ordered-alist.body
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@

asgc-ordered-alist-expandable.ads : asgc-ordered-alist.spec
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@

asgc-ordered-alist-expandable.adb : asgc-ordered-alist.body
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@

asgc-ordered-vector-fixed.ads : asgc-ordered-vector.spec
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@

asgc-ordered-vector-fixed.adb : asgc-ordered-vector.body
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@

asgc-ordered-vector-expandable.ads : asgc-ordered-vector.spec
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@

asgc-ordered-vector-expandable.adb : asgc-ordered-vector.body
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f unsortable.sed >$@


asgc-ordered-sortable-alist.ads : asgc-ordered-alist.ads
	sed -f sortable.sed $< >$@

asgc-ordered-sortable-dlist.ads : asgc-ordered-dlist.ads
	sed -f sortable.sed $< >$@

asgc-ordered-sortable-vector.ads : asgc-ordered-vector.ads
	sed -f sortable.sed $< >$@

asgc-ordered-sortable-dlist-dynamic.ads : asgc-ordered-dlist.spec
	sed -f dynamic.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-dlist-dynamic.adb : asgc-ordered-dlist.body
	sed -f dynamic.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-dlist-fixed.ads : asgc-ordered-dlist.spec
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-dlist-fixed.adb : asgc-ordered-dlist.body
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-dlist-expandable.ads : asgc-ordered-dlist.spec
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-dlist-expandable.adb : asgc-ordered-dlist.body
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-alist-fixed.ads : asgc-ordered-alist.spec
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-alist-fixed.adb : asgc-ordered-alist.body
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-alist-expandable.ads : asgc-ordered-alist.spec
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-alist-expandable.adb : asgc-ordered-alist.body
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-vector-fixed.ads : asgc-ordered-vector.spec
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-vector-fixed.adb : asgc-ordered-vector.body
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-vector-expandable.ads : asgc-ordered-vector.spec
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-vector-expandable.adb : asgc-ordered-vector.body
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-graph-dynamic-graph.ads : asgc-graph.spec
	sed -f dynamic.sed $< | sed -f graph.sed | sed -f unmanaged.sed >$@

asgc-graph-dynamic-graph.adb : asgc-graph.body
	sed -f dynamic.sed $< | sed -f graph.sed | sed -f unmanaged.sed >$@

asgc-graph-fixed-graph.ads : asgc-graph.spec
	sed -f fixed.sed $< | sed -f graph.sed | sed -f unmanaged.sed >$@

asgc-graph-fixed-graph.adb : asgc-graph.body
	sed -f fixed.sed $< | sed -f graph.sed | sed -f unmanaged.sed >$@

asgc-graph-expandable-graph.ads : asgc-graph.spec
	sed -f expandable.sed $< | sed -f graph.sed | sed -f unmanaged.sed >$@

asgc-graph-expandable-graph.adb : asgc-graph.body
	sed -f expandable.sed $< | sed -f graph.sed | sed -f unmanaged.sed >$@

asgc-graph-dynamic-digraph.ads : asgc-graph.spec
	sed -f dynamic.sed $< | sed -f unmanaged.sed  | sed -f digraph.sed >$@

asgc-graph-dynamic-digraph.adb : asgc-graph.body
	sed -f dynamic.sed $< | sed -f unmanaged.sed  | sed -f digraph.sed >$@

asgc-graph-fixed-digraph.ads : asgc-graph.spec
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f digraph.sed >$@

asgc-graph-fixed-digraph.adb : asgc-graph.body
	sed -f fixed.sed $< | sed -f unmanaged.sed  | sed -f digraph.sed >$@

asgc-graph-expandable-digraph.ads : asgc-graph.spec
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f digraph.sed >$@

asgc-graph-expandable-digraph.adb : asgc-graph.body
	sed -f expandable.sed $< | sed -f unmanaged.sed  | sed -f digraph.sed >$@


# Now the managed packages.
asgc-tree-dynamic_managed.ads : asgc-tree.spec
	sed -f dynamic.sed $< | sed -f managed.sed >$@

asgc-tree-dynamic_managed.adb : asgc-tree.body
	sed -f dynamic.sed $< | sed -f managed.sed >$@

asgc-tree-fixed_managed.ads : asgc-tree.spec
	sed -f fixed.sed $< | sed -f managed.sed >$@

asgc-tree-fixed_managed.adb : asgc-tree.body
	sed -f fixed.sed $< | sed -f managed.sed >$@

asgc-tree-expandable_managed.ads : asgc-tree.spec
	sed -f expandable.sed $< | sed -f managed.sed >$@

asgc-tree-expandable_managed.adb : asgc-tree.body
	sed -f expandable.sed $< | sed -f managed.sed >$@

asgc-list-dynamic_managed.ads : asgc-list.spec
	sed -f dynamic.sed $< | sed -f managed.sed >$@

asgc-list-dynamic_managed.adb : asgc-list.body
	sed -f dynamic.sed $< | sed -f managed.sed >$@

asgc-list-fixed_managed.ads : asgc-list.spec
	sed -f fixed.sed $< | sed -f managed.sed >$@

asgc-list-fixed_managed.adb : asgc-list.body
	sed -f fixed.sed $< | sed -f managed.sed >$@

asgc-list-expandable_managed.ads : asgc-list.spec
	sed -f expandable.sed $< | sed -f managed.sed >$@

asgc-list-expandable_managed.adb : asgc-list.body
	sed -f expandable.sed $< | sed -f managed.sed >$@

asgc-heap-dynamic_managed.ads : asgc-heap.spec
	sed -f dynamic.sed $< | sed -f managed.sed >$@

asgc-heap-dynamic_managed.adb : asgc-heap.body
	sed -f dynamic.sed $< | sed -f managed.sed >$@

asgc-heap-fixed_managed.ads : asgc-heap.spec
	sed -f fixed.sed $< | sed -f managed.sed >$@

asgc-heap-fixed_managed.adb : asgc-heap.body
	sed -f fixed.sed $< | sed -f managed.sed >$@

asgc-heap-expandable_managed.ads : asgc-heap.spec
	sed -f expandable.sed $< | sed -f managed.sed >$@

asgc-heap-expandable_managed.adb : asgc-heap.body
	sed -f expandable.sed $< | sed -f managed.sed >$@

asgc-btree-dynamic_managed.ads : asgc-btree-dynamic.spec
	sed -f managed.sed $< >$@

asgc-btree-dynamic_managed.adb : asgc-btree-dynamic.body
	sed -f managed.sed $< >$@

asgc-hash-dynamic_managed.ads : asgc-hash-dynamic.spec
	sed -f managed.sed $< >$@

asgc-hash-dynamic_managed.adb : asgc-hash-dynamic.body
	sed -f managed.sed $< >$@

asgc-hash-fixed_managed.ads : asgc-hash-fixed.spec
	sed -f managed.sed $< >$@

asgc-hash-fixed_managed.adb : asgc-hash-fixed.body
	sed -f managed.sed $< >$@

asgc-hash-expandable_managed.ads : asgc-hash-expandable.spec
	sed -f managed.sed $< >$@

asgc-hash-expandable_managed.adb : asgc-hash-expandable.body
	sed -f managed.sed $< >$@

asgc-ordered-dlist-dynamic_managed.ads : asgc-ordered-dlist.spec
	sed -f dynamic.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@

asgc-ordered-dlist-dynamic_managed.adb : asgc-ordered-dlist.body
	sed -f dynamic.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@

asgc-ordered-dlist-fixed_managed.ads : asgc-ordered-dlist.spec
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@

asgc-ordered-dlist-fixed_managed.adb : asgc-ordered-dlist.body
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@

asgc-ordered-dlist-expandable_managed.ads : asgc-ordered-dlist.spec
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@

asgc-ordered-dlist-expandable_managed.adb : asgc-ordered-dlist.body
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@

asgc-ordered-alist-fixed_managed.ads : asgc-ordered-alist.spec
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@

asgc-ordered-alist-fixed_managed.adb : asgc-ordered-alist.body
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@

asgc-ordered-alist-expandable_managed.ads : asgc-ordered-alist.spec
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@

asgc-ordered-alist-expandable_managed.adb : asgc-ordered-alist.body
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@

asgc-ordered-vector-fixed_managed.ads : asgc-ordered-vector.spec
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@

asgc-ordered-vector-fixed_managed.adb : asgc-ordered-vector.body
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@

asgc-ordered-vector-expandable_managed.ads : asgc-ordered-vector.spec
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@

asgc-ordered-vector-expandable_managed.adb : asgc-ordered-vector.body
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f unsortable.sed >$@


asgc-ordered-sortable-dlist-dynamic_managed.ads : asgc-ordered-dlist.spec
	sed -f dynamic.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-dlist-dynamic_managed.adb : asgc-ordered-dlist.body
	sed -f dynamic.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-dlist-fixed_managed.ads : asgc-ordered-dlist.spec
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-dlist-fixed_managed.adb : asgc-ordered-dlist.body
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-dlist-expandable_managed.ads : asgc-ordered-dlist.spec
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-dlist-expandable_managed.adb : asgc-ordered-dlist.body
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-alist-fixed_managed.ads : asgc-ordered-alist.spec
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-alist-fixed_managed.adb : asgc-ordered-alist.body
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-alist-expandable_managed.ads : asgc-ordered-alist.spec
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-alist-expandable_managed.adb : asgc-ordered-alist.body
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-vector-fixed_managed.ads : asgc-ordered-vector.spec
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-vector-fixed_managed.adb : asgc-ordered-vector.body
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-vector-expandable_managed.ads : asgc-ordered-vector.spec
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-ordered-sortable-vector-expandable_managed.adb : asgc-ordered-vector.body
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f sortable.sed | sed -f fixup.sed >$@

asgc-graph-dynamic-graph_managed.ads : asgc-graph.spec
	sed -f dynamic.sed $< | sed -f graph.sed | sed -f managed.sed >$@

asgc-graph-dynamic-graph_managed.adb : asgc-graph.body
	sed -f dynamic.sed $< | sed -f graph.sed | sed -f managed.sed >$@

asgc-graph-fixed-graph_managed.ads : asgc-graph.spec
	sed -f fixed.sed $< | sed -f graph.sed | sed -f managed.sed >$@

asgc-graph-fixed-graph_managed.adb : asgc-graph.body
	sed -f fixed.sed $< | sed -f graph.sed | sed -f managed.sed >$@

asgc-graph-expandable-graph_managed.ads : asgc-graph.spec
	sed -f expandable.sed $< | sed -f graph.sed | sed -f managed.sed >$@

asgc-graph-expandable-graph_managed.adb : asgc-graph.body
	sed -f expandable.sed $< | sed -f graph.sed | sed -f managed.sed >$@

asgc-graph-dynamic-digraph_managed.ads : asgc-graph.spec
	sed -f dynamic.sed $< | sed -f managed.sed  | sed -f digraph.sed >$@

asgc-graph-dynamic-digraph_managed.adb : asgc-graph.body
	sed -f dynamic.sed $< | sed -f managed.sed  | sed -f digraph.sed >$@

asgc-graph-fixed-digraph_managed.ads : asgc-graph.spec
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f digraph.sed >$@

asgc-graph-fixed-digraph_managed.adb : asgc-graph.body
	sed -f fixed.sed $< | sed -f managed.sed  | sed -f digraph.sed >$@

asgc-graph-expandable-digraph_managed.ads : asgc-graph.spec
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f digraph.sed >$@

asgc-graph-expandable-digraph_managed.adb : asgc-graph.body
	sed -f expandable.sed $< | sed -f managed.sed  | sed -f digraph.sed >$@


# Make the graph link packages
asgc-graph-links-dynamic.ads : asgc-graph-links-dynamic.spec
	sed -f unmanaged.sed $< >$@

asgc-graph-links-dynamic_managed.ads : asgc-graph-links-dynamic.spec
	sed -f managed.sed $< >$@

asgc-graph-links-dynamic.adb : asgc-graph-links-dynamic.body
	sed -f unmanaged.sed $< >$@

asgc-graph-links-dynamic_managed.adb : asgc-graph-links-dynamic.body
	sed -f managed.sed $< >$@

asgc-graph-links-expandable.ads : asgc-graph-links-expandable.spec
	sed -f unmanaged.sed $< >$@

asgc-graph-links-expandable_managed.ads : asgc-graph-links-expandable.spec
	sed -f managed.sed $< >$@

asgc-graph-links-expandable.adb : asgc-graph-links-expandable.body
	sed -f unmanaged.sed $< >$@

asgc-graph-links-expandable_managed.adb : asgc-graph-links-expandable.body
	sed -f managed.sed $< >$@

asgc-graph-links-fixed.ads : asgc-graph-links-fixed.spec
	cp $< $@

asgc-graph-links-fixed.adb : asgc-graph-links-fixed.body
	cp $< $@


krunch : gen
	ls asgc-*.ad? asoc-*.ad? >tokrunch
	awk -f filetr.awk tokrunch >filetr
	awk -f makeadc.awk filetr >gnat.adc
	awk -f krit.awk filetr >krit
	sh krit
	cat $(CHOPPED_FILES) >t1
	rm $(CHOPPED_FILES)
	gnatchop -k t1
	rm t1 tokrunch filetr krit
	zip gensrc $(GEN_SOURCE)
	rm $(GEN_SOURCE)
	(cd tests; make krunch)
	(cd examples; make krunch)
	cp gnat.adc tests
	cp gnat.adc examples

Overview.ps : Overview.ltx
	latex Overview.ltx
	latex Overview.ltx
	dvips -o Overview.ps Overview.dvi
	rm -f Overview.aux Overview.dvi Overview.log Overview.toc

Overview.pdf : Overview.ps
	ps2pdf Overview.ps

Overview : Overview.ltx
	latex2html Overview.ltx

doc : Overview.ps Overview.pdf Overview


ASGC_SOURCES = asgc-btree.ads asgc-graph-dynamic.ads \
	asgc-graph-expandable.ads asgc-graph-fixed.ads asgc-graph-links.ads \
	asgc-graph.adb asgc-graph.ads asgc-hash.ads asgc-heap.ads \
	asgc-list.adb asgc-list.ads asgc-ordered-alist.ads \
	asgc-ordered-dlist.ads asgc-ordered-sortable-bubble_sort.adb \
	asgc-ordered-sortable-bubble_sort.ads \
	asgc-ordered-sortable-quicksort.adb \
	asgc-ordered-sortable-quicksort.ads \
	asgc-ordered-sortable.adb asgc-ordered-sortable.ads \
	asgc-ordered-vector.ads asgc-ordered.adb asgc-ordered.ads \
	asgc-setops.adb asgc-setops.ads asgc-tree.ads asgc.adb asgc.ads

ASL_SOURCES = asl-leak_detect_pool.adb asl-leak_detect_pool.ads \
	asl-semaphore-binary.adb asl-semaphore-binary.ads \
	asl-semaphore-counting.adb asl-semaphore-counting.ads \
	asl-semaphore-nested.adb asl-semaphore-nested.ads \
	asl-semaphore-nested_prio.adb asl-semaphore-nested_prio.ads \
	asl-semaphore.ads asl-strings.ads asl-strings.adb \
	asl-strings-tokenizer.ads \
	asl-strings-tokenizer.adb asl-abstract_io.ads asl-abstract_io.adb \
	asl-abstract_io-telnet.ads asl-abstract_io-telnet.adb \
	asl-refcount_ptr.ads asl-refcount_ptr.adb \
	asl-refcount_string.ads asl-refcount_string.adb \
	asl-refcount_ptr-managed.ads asl-refcount_ptr-managed.adb \
	asl-refcount_string-managed.ads asl-refcount_string-managed.adb \
	asl-date_time.ads asl-date_time.adb \
	asl-date_time-calendar.ads asl-date_time-calendar.adb \
	asl-date_time-timezone.ads asl-date_time-timezone.adb \
	asl-date_time-timezone-simple.ads asl-date_time-timezone-simple.adb \
        asl-date_time-timezone-complex.ads asl-date_time-timezone-complex.adb \
	asl-date_time-local.ads asl-date_time-local.adb

ASP_SOURCES = asp.ads asp-logging.ads asp-logging.adb

ASOC_SOURCES = asoc-btree_o-dynamic_o.ads asoc-btree_o.ads \
	asoc-graph_o-dynamic_o.ads asoc-graph_o-expandable_o.ads \
	asoc-graph_o-fixed_o.ads asoc-graph_o.ads \
	asoc-hash_o-dynamic_o.ads asoc-hash_o-expandable_o.ads \
	asoc-hash_o-fixed_o.ads asoc-hash_o.ads asoc-heap_o-dynamic_o.ads \
	asoc-heap_o-expandable_o.ads asoc-heap_o-fixed_o.ads \
	asoc-heap_o.ads asoc-list_o-dynamic_o.ads \
	asoc-list_o-expandable_o.ads asoc-list_o-fixed_o.ads \
	asoc-list_o.ads asoc-ordered_o-alist_o-expandable_o.ads \
	asoc-ordered_o-alist_o-fixed_o.ads asoc-ordered_o-alist_o.ads \
	asoc-ordered_o-dlist_o-dynamic_o.ads \
	asoc-ordered_o-dlist_o-expandable_o.ads \
	asoc-ordered_o-dlist_o-fixed_o.ads \
	asoc-ordered_o-dlist_o.ads \
	asoc-ordered_o-sortable_o-alist_o-expandable_o.ads \
	asoc-ordered_o-sortable_o-alist_o-fixed_o.ads \
	asoc-ordered_o-sortable_o-alist_o.ads \
	asoc-ordered_o-sortable_o-dlist_o-dynamic_o.ads \
	asoc-ordered_o-sortable_o-dlist_o-expandable_o.ads \
	asoc-ordered_o-sortable_o-dlist_o-fixed_o.ads \
	asoc-ordered_o-sortable_o-dlist_o.ads \
	asoc-ordered_o-sortable_o-vector_o-expandable_o.ads \
	asoc-ordered_o-sortable_o-vector_o-fixed_o.ads \
	asoc-ordered_o-sortable_o-vector_o.ads \
	asoc-ordered_o-sortable_o.ads \
	asoc-ordered_o-vector_o-expandable_o.ads \
	asoc-ordered_o-vector_o-fixed_o.ads \
	asoc-ordered_o-vector_o.ads asoc-ordered_o.ads asoc-setops_o.ads \
	asoc-tree_o-dynamic_o.ads asoc-tree_o-expandable_o.ads \
	asoc-tree_o-fixed_o.ads asoc-tree_o.ads asoc.ads asoc_collectible.adb \
	asoc_collectible.ads

ASL_TK_SOURCES = asl-tcl.adb asl-tcl.ads asl-tk-button.adb asl-tk-button.ads \
	asl-tk-checkbutton.adb asl-tk-checkbutton.ads \
	asl-tk-frame-toplevel.adb asl-tk-frame-toplevel.ads \
	asl-tk-frame.adb asl-tk-frame.ads asl-tk-grid.adb asl-tk-grid.ads \
	asl-tk-image.adb asl-tk-image.ads asl-tk-label.adb asl-tk-label.ads \
	asl-tk-menubar.adb asl-tk-menubar.ads asl-tk-menubutton.adb \
	asl-tk-menubutton.ads asl-tk-menuitem-button.adb \
	asl-tk-menuitem-button.ads asl-tk-menuitem-cascade.adb \
	asl-tk-menuitem-cascade.ads asl-tk-menuitem-checkbutton.adb \
	asl-tk-menuitem-checkbutton.ads asl-tk-menuitem.adb \
	asl-tk-menuitem.ads asl-tk-radiobutton.adb asl-tk-radiobutton.ads \
	asl-tk.adb asl-tk.ads asl.ads \
	gui.ads

TK_TEST_SOURCES = test_tk.adb test_tk_callback.adb test_tk_callback.ads

ASL_PROTOCOL_SOURCES = asl-protocol-telnet-option-status.adb \
	asl-protocol-telnet-option.adb \
	asl-protocol-telnet-option-status.ads \
	asl-protocol-telnet-option.ads \
	asl-protocol-telnet-option-terminal_type.adb \
	asl-protocol-telnet-stream_io.adb \
	asl-protocol-telnet-option-terminal_type.ads \
	asl-protocol-telnet-stream_io.ads \
	asl-protocol-telnet-option-window_size.adb \
	asl-protocol-telnet.adb \
	asl-protocol-telnet-option-window_size.ads \
	asl-protocol-telnet.ads \
	asl-protocol.ads \
	asl-cmdproc.ads asl-cmdproc.adb \
	asl-cmdproc-telnet.ads asl-cmdproc-telnet.adb \
	asl-security.ads asl-security-userpass.ads asl-security-userpass.adb \
	asl-debug_out.ads asl-debug_out.adb \
	asl-debug_out-command.ads asl-debug_out-command.adb

MISC_SOURCES = baseclass.adb baseclass.ads cargv.adb cargv.ads \
	asoc_cmp1.ads asoc_cmp2.ads asoc_cmp_managed.ads asl_compile.ads \
	socket_compile.ads findzone.c



DIST_FILES = COPYING ChangeLog Changelog Makefile Overview.ltx README \
	README.Tk TODO license.htm Overview.pdf \
	$(GEN_SOURCE) $(ASGC_SOURCES) $(ASL_SOURCES) $(ASOC_SOURCES) \
	$(ASL_TK_SOURCES) $(MISC_SOURCES) $(TK_TEST_SOURCES) \
	$(ASL_PROTOCOL_SOURCES) $(TOOL_FILES) $(TIMEZONE_FILES) \
	$(OTHER_ZONE_FILES) $(ASP_SOURCES)

EXAMPLE_FILES = Makefile README \
	asu_components.adb asu_components.ads count_words.adb \
	demo_graph1.adb demo_graph1_package.adb demo_graph1_package.ads \
	state_graph.adb state_graph.ads state_iterator_vector.adb \
	state_iterator_vector.ads state_path.adb state_stack.ads \
	word_parser.adb word_parser.ads \
	telnet_server.adb telnet_server_handlers.adb \
	telnet_server_handlers.ads telnet_cmdproc.adb \
	telnet_cmdproc_handlers.ads telnet_cmdproc_handlers.adb

TEST_FILES = Makefile \
	test_btree.adb test_btree_random.adb test_digraph.adb test_graph.adb \
	test_hash.adb test_hash_random.adb test_heap.adb test_heap_random.adb \
	test_list.adb test_ordered.adb test_ordered_sort.adb test_p-btree.ads \
	test_p-digraph.adb test_p-digraph.ads test_p-graph-digraph.ads \
	test_p-graph-graph.ads test_p-graph.adb test_p-graph.ads \
	test_p-hash.ads test_p-heap.ads test_p-list.ads test_p-ordered.ads \
	test_p-sortable.ads test_p-tree.ads test_p.adb test_p.ads \
	test_semaphore.adb test_sets.adb test_tree.adb test_tree_balanced.adb \
	test_tree_balanced_random.adb \
	test_telnet.adb test_telnet_handlers.ads test_telnet_handlers.adb \
	test_string_tokenizer.adb test_abstract_io.adb \
	test_abstract_io_handlers.ads test_abstract_io_handlers.adb \
	test_refcount_ptr.adb test_refcount_pack.ads \
	test_calendar.adb

conv_zones: conv_zones.adb conv_zones_data.adb conv_zones_data.ads
	gnatmake conv_zones

asl-date_time-register_simple_timezones.adb: $(TIMEZONE_FILES) conv_zones
	cat $(TIMEZONE_FILES) | ./conv_zones simple >$@

asl-date_time-register_complex_timezones.adb: $(TIMEZONE_FILES) conv_zones
	cat $(TIMEZONE_FILES) | ./conv_zones complex >$@

zonetab.h: zonetab.awk Timezones/zone.tab
	awk -f zonetab.awk <Timezones/zone.tab >$@

findzone.o: findzone.c zonetab.h
	gnatgcc -c $(CFLAGS) findzone.c

dist: $(DIST_FILES)
	rm -rf adasl-$(VERSION)
	mkdir adasl-$(VERSION)
	mkdir adasl-$(VERSION)/tests
	mkdir adasl-$(VERSION)/examples
	mkdir adasl-$(VERSION)/Timezones
	cp $(DIST_FILES) adasl-$(VERSION)
	(cd tests; cp $(TEST_FILES) ../adasl-$(VERSION)/tests)
	(cd examples; cp $(EXAMPLE_FILES) ../adasl-$(VERSION)/examples)
	cp $(TIMEZONE_FILES) $(OTHER_ZONE_FILES) adasl-$(VERSION)/Timezones
	tar czf adasl-$(VERSION).tar.gz adasl-$(VERSION)
	rm -rf adasl-$(VERSION)

clean::
	rm -f *.o *.ali $(TESTS) $(C_S) $(GENNED_FILES) adasl-$(VERSION) \
		adasl-$(VERSION).tar.gz
	rm -f Overview.aux Overview.dvi Overview.ps Overview.pdf Overview.log \
		Overview.toc
	rm -rf Overview conv_zones zonetab.h
	(cd tests; make clean)
	(cd examples; make clean)
