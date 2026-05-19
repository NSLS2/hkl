##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
m4_define([v_maj], [5])
m4_define([v_min], [1])
m4_define([v_mic], [9])
m4_define([v_rev], m4_esyscmd([(git rev-list --count HEAD 2>/dev/null || echo 0) | tr -d '\n']))dnl
##--   When released, remove the dnl on the below line
m4_undefine([v_rev])
##--   When doing snapshots - change soname. remove dnl on below line
dnl m4_define([relname], [ver-pre-svn-07])
dnl m4_define([v_rel], [-release relname])
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
m4_ifdef([v_rev], [m4_define([v_ver], [v_maj.v_min.v_mic.v_rev])], [m4_define([v_ver], [v_maj.v_min.v_mic])])
m4_define([lt_cur], m4_eval(v_maj + v_min))
m4_define([lt_rev], v_mic)
m4_define([lt_age], v_min)
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

AC_PREREQ([2.72])
AC_INIT([hkl],[v_ver],[picca@synchrotron-soleil.fr])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR(config)

AM_INIT_AUTOMAKE([silent-rules tar-ustar -Wno-portability subdir-objects])

# Checks for programs.
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CXX
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB
LT_INIT
AM_SILENT_RULES

##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
m4_ifdef([v_rev], , [m4_define([v_rev], [0])])
m4_ifdef([v_rel], , [m4_define([v_rel], [])])
AC_DEFINE_UNQUOTED(VMAJ, [v_maj], [Major version])
AC_DEFINE_UNQUOTED(VMIN, [v_min], [Minor version])
AC_DEFINE_UNQUOTED(VMIC, [v_mic], [Micro version])
AC_DEFINE_UNQUOTED(VREV, [v_rev], [Revison])
version_info="lt_cur:lt_rev:lt_age"
release_info="v_rel"
AC_SUBST(version_info)
AC_SUBST(release_info)
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
VMAJ=v_maj
AC_SUBST(VMAJ)

# activate all the warnings
AX_CFLAGS_WARN_ALL
AX_CXXFLAGS_WARN_ALL

# Checks for libraries.
AX_PATH_GSL
AM_PATH_GLIB_2_0

# Checks for header files.
AC_FUNC_ALLOCA
AC_CHECK_HEADERS_ONCE([fcntl.h float.h limits.h malloc.h memory.h stddef.h stdint.h stdlib.h string.h sys/param.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INT8_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_VPRINTF
AC_FUNC_FORK
AC_CHECK_FUNCS([atexit dup2 fchdir floor getcwd getpagesize gettimeofday isascii memchr memmove memset mkdir pow putenv rmdir select sqrt strchr strcasecmp strdup strerror strncasecmp strrchr strstr strtol strtoul])

dnl OPTION_DEFAULT_OFF(NAME, HELP-STRING)
dnl Create a new --enable option that defaults to being disabled.
dnl NAME is the base name of the option.  The shell variable enable_NAME
dnl   will be set to either the user's value (if the option is
dnl   specified; 'yes' for a plain --enable-NAME) or to 'no' (if the
dnl   option is not specified).  Note that the shell variable name is
dnl   constructed as autoconf does, by replacing non-alphanumeric
dnl   characters with "_".
dnl HELP-STRING is the help text for the option.
AC_DEFUN([OPTION_DEFAULT_OFF], [dnl
  AC_ARG_ENABLE([$1],[AS_HELP_STRING([--enable-$1],[$2])],[],[dnl
    m4_bpatsubst([enable_$1], [[^0-9a-z]], [_])=no])dnl
])dnl

dnl OPTION_DEFAULT_ON(NAME, HELP-STRING)
dnl Create a new --enable option that defaults to being enabled.
dnl NAME is the base name of the option.  The shell variable enable_NAME
dnl   will be set to either the user's value (if the option is
dnl   specified; 'no' for a plain --enable-NAME) or to 'yes' (if the
dnl   option is not specified).  Note that the shell variable name is
dnl   constructed as autoconf does, by replacing non-alphanumeric
dnl   characters with "_".
dnl HELP-STRING is the help text for the option.
AC_DEFUN([OPTION_DEFAULT_ON], [dnl
  AC_ARG_ENABLE([$1],[AS_HELP_STRING([--disable-$1],[$2])],[],[dnl
    m4_bpatsubst([enable_$1], [[^0-9a-z]], [_])=yes])dnl
])dnl


dnl ****************
dnl *** analyzer ***
dnl ****************

OPTION_DEFAULT_OFF([analyzer], [enable gcc-10 -fanalyzer])
AM_CONDITIONAL([ANALYZER], [test x$enable_analyzer != xno])
AM_COND_IF([ANALYZER],
        AX_CHECK_COMPILE_FLAG([-fanalyzer], [CFLAGS="$CFLAGS -fanalyzer"], [], [])
)

dnl ***************
dnl *** logging ***
dnl ***************

OPTION_DEFAULT_OFF([logging], [enable the glib logging])
AM_CONDITIONAL([LOGGING], [test x$enable_logging != xno])
AM_COND_IF([LOGGING],
        AC_DEFINE([LOGGING], [1], [activate the logging or not])
        )

dnl ***********
dnl *** doc ***
dnl ***********

OPTION_DEFAULT_ON([hkl-doc], [build documentation])
AM_CONDITIONAL([HKL_DOC], [test x$enable_hkl_doc = xyes])
AM_COND_IF([HKL_DOC],
           [dnl *** asymptote ***
            AC_PATH_PROG([ASY], [asy], [echo])
            dnl *** emacs ***
            AC_PATH_PROG([EMACS], [emacs], [no])
            dnl *** gnuplot ***
            AC_PATH_PROG([GNUPLOT], [gnuplot], [echo])
            dnl *** xvfb-run ***
            AC_PATH_PROG([XVFB_RUN], [xvfb-run], [echo])
           ])

dnl *** mathkjax ***
AC_ARG_WITH([mathjax],
            [AS_HELP_STRING([--with-mathjax],[use mathjax (default is "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML")])],
            [use_mathjax=$withval],
            [use_mathjax="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML"])

MATHJAX_PATH=$use_mathjax
AC_SUBST(MATHJAX_PATH)

dnl *** gtk-doc ***
GTK_DOC_CHECK([1.9],[--flavour no-tmpl])

dnl **********************************************
dnl *** add an option for the binoculars build ***
dnl **********************************************

OPTION_DEFAULT_ON([binoculars], [do not compile binoculars-ng])

AM_CONDITIONAL([BINOCULARS], [test x$enable_binoculars = xyes])
AM_COND_IF([BINOCULARS],
           [PKG_CHECK_MODULES([HDF5], [hdf5-serial >= 1.8.13])
            PKG_CHECK_MODULES([CGLM], [cglm >= 0.7])
            AC_PATH_PROG([CABAL], [cabal])
            if test -z "$CABAL" ; then
               AC_MSG_ERROR([cabal was not found])
            fi
])

AC_SUBST(CABALFLAGS)

dnl *******************************
dnl *** add an option for hkl3d ***
dnl *******************************

OPTION_DEFAULT_OFF([hkl3d], [compile the hkl3d library])

AM_CONDITIONAL([HKL3D], [test x$enable_hkl3d != xno])
AM_COND_IF([HKL3D],
           [PKG_CHECK_MODULES([ASSIMP], [assimp >= 0])
            PKG_CHECK_MODULES([BULLET], [bullet >= 2.82])
            dnl *** blender ***
            AC_PATH_PROG([BLENDER], [blender], [no])
            AC_PATH_PROG([BLENDER_THUMBNAILER], [blender-thumbnailer], [no])
            dnl check for yaml
            AC_SEARCH_LIBS([yaml_get_version], [yaml],
                           [AC_CHECK_HEADERS([yaml.h], [],
                            [AC_MSG_ERROR([Could not find the yaml headers, please install yaml developement files])]
                           )],
                           [AC_MSG_ERROR([Could not find the libyaml runtime, please install yaml])])
            YAML_CFLAGS=""
            AC_SUBST(YAML_CFLAGS)
            YAML_LIBS="-lyaml"
            AC_SUBST(YAML_LIBS)])

dnl ****************************************
dnl *** add an option for the ghkl build ***
dnl ****************************************

OPTION_DEFAULT_ON([gui], [do not compile the gui interface])

AM_CONDITIONAL([GUI], [test x$enable_gui = xyes])
AM_COND_IF([GUI],
        [PKG_CHECK_MODULES([GTK], [gtk4 >= 4.12])
        AM_COND_IF([HKL3D],
                   [PKG_CHECK_MODULES([EPOXY], [epoxy >= 1.5.10])
        ])
])

dnl *********************
dnl *** introspection ***
dnl *********************

GOBJECT_INTROSPECTION_CHECK([0.6.7])

AM_COND_IF([HAVE_INTROSPECTION],
        [PKG_CHECK_MODULES([GOBJECT], [gobject-2.0])])


dnl ***************
dnl *** contrib ***
dnl ***************

OPTION_DEFAULT_OFF([contrib], [compile the contrib part])

AM_CONDITIONAL([CONTRIB], [test x$enable_contrib != xno])


dnl ******************
dnl *** datatype99 ***
dnl ******************

OPTION_DEFAULT_OFF([datatype99], [use the system datatype99])
AC_CHECK_HEADERS([datatype99.h], [], [enable_datatype99=no])

DATATYPE99_CFLAGS="-ftrack-macro-expansion=0"
AM_CONDITIONAL([DATATYPE99], [test x$enable_datatype99 != xno])
AM_COND_IF([DATATYPE99],
        [],
        [DATATYPE99_CFLAGS=$DATATYPE99_CFLAGS' -I$(top_srcdir)/third-party/']
        )
AC_SUBST(DATATYPE99_CFLAGS)


dnl ***************
dnl *** openssl ***
dnl ***************

PKG_CHECK_MODULES([OPENSSL], [openssl >= 3])

AC_CONFIG_FILES([Makefile
                 binoculars-ng/binoculars/Makefile
                 binoculars-ng/hkl.cabal
                 binoculars-ng/Makefile
                 hkl/Makefile
                 hkl/api2/Makefile
                 hkl/ccan/Makefile
                 hkl.h
                 hkl.pc
                 hkl3d/Makefile
                 hkl3d.pc
                 tests/Makefile
                 tests/bindings/Makefile
                 tests/tap/Makefile
                 third-party/Makefile
                 Documentation/Makefile
                 Documentation/hkl.org
                 Documentation/api/Makefile
                 Documentation/figures/Makefile
                 Documentation/sphinx/Makefile
                 Documentation/sphinx/source/conf.py
                 gui/Makefile
                 data/Makefile
                 contrib/Makefile
                 contrib/cristal/Makefile
                 contrib/cristal/xrays/Makefile
                 contrib/sixs/Makefile
                 contrib/petra3/Makefile
])

AC_OUTPUT
