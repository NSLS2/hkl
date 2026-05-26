AM_CPPFLAGS = -Wextra -D_DEFAULT_SOURCE \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	$(DATATYPE99_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)

LDADD = \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	$(top_builddir)/hkl/libhkl-private.la

noinst_LTLIBRARIES=libhkl2.la
libhkl2_la_SOURCES = \
	hkl2.c \
	hkl2.h
