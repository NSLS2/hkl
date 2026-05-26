SUBDIRS = tap .
if HAVE_INTROSPECTION
SUBDIRS += bindings
endif

all_tests = \
	hkl-unit-t \
	hkl-bench-t \
	hkl-axis-t \
	hkl-quaternion-t \
	hkl-interval-t \
	hkl-sample-t \
	hkl-lattice-t \
	hkl-source-t \
	hkl-detector-t \
	hkl-matrix-t \
	hkl-vector-t \
	hkl-geometry-t \
	hkl-parameter-t \
	hkl-pseudoaxis-t \
	hkl-pseudoaxis-e4cv-t \
	hkl-pseudoaxis-e4ch-t \
	hkl-pseudoaxis-e6c-t \
	hkl-pseudoaxis-k4cv-t \
	hkl-pseudoaxis-k6c-t \
	hkl-pseudoaxis-soleil-nanoscopium-robot-t \
	hkl-pseudoaxis-soleil-sirius-kappa-t \
	hkl-pseudoaxis-soleil-sixs-med-t \
	hkl-pseudoaxis-zaxis-t

AM_CPPFLAGS = -Wextra -D_DEFAULT_SOURCE \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-I$(top_srcdir)/tests \
	$(DATATYPE99_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GOBJECT_CFLAGS) \
	$(GSL_CFLAGS)

LDADD = $(top_builddir)/hkl/libhkl-private.la \
	$(top_builddir)/hkl/api2/libhkl2.la \
	$(top_builddir)/tests/tap/libtap.a \
	$(GLIB_LIBS) \
	$(GOBJECT_LIBS) \
	$(GSL_LIBS)

if BINOCULARS

all_tests += hkl-binoculars-t

AM_CPPFLAGS += -I$(top_srcdir)/binoculars-ng/binoculars \
		$(OPENSSL_CFLAGS)

LDADD += $(top_builddir)/binoculars-ng/binoculars/libhkl-binoculars.la \
	$(OPENSSL_LIBS)

endif


if HKL3D

all_tests += hkl3d-test-t

# force linkage with g++
nodist_EXTRA_hkl3d_test_t_SOURCES = dummy.cxx

AM_CPPFLAGS += -I$(top_srcdir)/hkl3d \
	$(G3D_CFLAGS) \
	$(BULLET_CFLAGS)

LDADD += $(top_builddir)/hkl3d/libhkl3d.la \
	$(BULLET_LIBS)

endif

check_PROGRAMS = runtests $(all_tests)

## tap tests

check-local: $(check_PROGRAMS)
	./runtests -v -s $(abs_srcdir) $(all_tests)

valgrind:
	G_DEBUG=gc-friendly G_SLICE=always-malloc valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes .libs/hkl-bench-t

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax valgrind
