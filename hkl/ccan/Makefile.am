AM_CFLAGS=-I$(top_srcdir)

noinst_PROGRAMS=configurator
BUILT_SOURCES=ccan_config.h
ccan_config.h: configurator
	$(builddir)/configurator $(CC) $(CCAN_CFLAGS) > $@.tmp && mv $@.tmp $@
noinst_LTLIBRARIES=libccan.la
libccan_la_SOURCES = \
	ccan_config.h \
	alignof/alignof.h \
	array_size/array_size.h \
	build_assert/build_assert.h \
	check_type/check_type.h \
	compiler/compiler.h \
	container_of/container_of.h \
	coroutine/coroutine.c \
	coroutine/coroutine.h \
	cppmagic/cppmagic.h \
	darray/darray.h \
	generator/generator.c \
	generator/generator.h \
	list/list.c \
	list/list.h \
	ptrint/ptrint.h \
	str/debug.c \
	str/str.c \
	str/str_debug.h \
	str/str.h \
	typesafe_cb/typesafe_cb.h

ccanincludedir=$(includedir)/hkl-@VMAJ@/hkl/ccan
nobase_ccaninclude_HEADERS= \
	darray/darray.h
nodist_ccaninclude_HEADERS=ccan_config.h
EXTRA_DIST=configurator.c
CLEANFILES=configurator ccan_config.h
