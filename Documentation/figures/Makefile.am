XVFB_FLAGS=-s "-screen 0 1024x768x24 -ac +extension GLX +render -noreset"

.asy.png:
	-LIBGL_ALWAYS_SOFTWARE=true @XVFB_RUN@ $(XVFB_FLAGS) @ASY@ -config "" -d -render=4 -f png -o $(@:.png=) $<

.asy.pdf:
	--LIBGL_ALWAYS_SOFTWARE=true @XVFB_RUN@ $(XVFB_FLAGS) @ASY@ -config "" -d -render=0 -f pdf -noprc -o $@ $<

.gnuplot.svg:
	-@GNUPLOT@ $<

AM_CPPFLAGS = -Wextra -D_DEFAULT_SOURCE \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	$(DATATYPE99_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GOBJECT_CFLAGS) \
	$(GSL_CFLAGS)

LDADD = \
	$(top_builddir)/hkl/api2/libhkl2.la \
	$(top_builddir)/hkl/libhkl-private.la \
	$(GLIB_LIBS) \
	$(GOBJECT_LIBS) \
	$(GSL_LIBS)

noinst_PROGRAMS = sirius

SIRIUSFILES=s1-11.dat s2-11.dat s1-101.dat s2-101.dat m1-11.dat m2-11.dat m1-101.dat m2-101.dat m3-100.dat traj_n.dat
SVGFILES=sirius-s.svg sirius-m.svg sirius-m-zoom.svg traj_n.svg sirius-m3-100.svg

$(SVGFILES): $(SIRIUSFILES)

sirius-stamp: sirius
	@rm -f sirius-temp
	@touch sirius-temp
	cd $(builddir) && ./sirius
	@mv -f sirius-temp $@

$(SIRIUSFILES): sirius-stamp
	## Recover from the removal of $@
	@if test -f $@; then :; else \
		trap 'rm -rf sirius.lock sirius.stamp' 1 2 13 15; \
		## mkdir is a portable test-and-set
		if mkdir sirius.lock 2>/dev/null; then \
		## This code is being executed by the first process.
			rm -f sirius.stamp; \
			$(MAKE) $(AM_MAKEFLAGS) sirius.stamp; \
		else \
		## This code is being executed by the follower processes.
		## Wait until the first process is done.
			while test -d sirius.lock; do sleep 1; done; \
		## Succeed if and only if the first process succeeded.
			test -f sirius.stamp; exit $$?; \
		fi; \
	fi

dist_noinst_DATA = \
	3S+1D.png \
	B_b3_y.png \
	B_a.png \
	B_b.png \
	crystal.png \
	4S+2D.png \
	4S+2D_reciprocal.png \
	4S+2D_reciprocal2.png \
	4S+2D_reciprocal3.png \
	4S+2D_reciprocal4.png \
	ghkl.png \
	ghkl-collision.png \
	k4cv.png \
	k6c.png \
	med2_3_slits.png \
	zaxis.png \
	e2k_1.png \
	e2k_2.png \
	e2k_3.png \
	e2k_4.png \
	qper_qpar.png \
	$(SIRIUSFILES) \
	$(SVGFILES)

CLEANFILES = $(SIRIUSFILES) $(SVGFILES) sirius-stamp

MAINTAINERCLEANFILES = $(dist_noinst_DATA)

EXTRA_DIST = \
	3S+1D.asy \
	B_b3_y.asy \
	B_a.asy \
	B_b.asy \
	crystal.asy \
	4S+2D.asy \
	4S+2D_reciprocal.asy \
	4S+2D_reciprocal2.asy \
	4S+2D_reciprocal3.asy \
	4S+2D_reciprocal4.asy \
	ghkl.png \
	ghkl-collision.png \
	k4cv.asy \
	k6c.asy \
	med2_3_slits.asy \
	diffractometer.asy \
	zaxis.asy \
	eulerians.asy \
	e2k_1.asy \
	e2k_2.asy \
	e2k_3.asy \
	e2k_4.asy \
	qper_qpar.asy \
	sirius-s.gnuplot \
	sirius-m.gnuplot \
	sirius-m-zoom.gnuplot \
	sirius-m3-100.gnuplot \
	traj_n.gnuplot

# Support for GNU Flymake, in Emacs.

check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)
