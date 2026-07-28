/* This file is part of the hkl library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2003-2019 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 * Copyright (C) 2026      Brookhaven National Laboratory
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          Thomas A Caswell <tcaswell@bnl.gov>
 */

/* NSLS-II CDI 3-2-2 diffractometer.
 *
 * The instrument has a 5-circle sample stack (mu carries omega
 * carries chi carries a second omega/chi pair, named omega2/chi2
 * here) and two independent detector arms, each carrying 2 axes
 * (tth, phi).  A libhkl geometry supports only a single detector
 * holder, so this geometry describes the sample stack plus ONE
 * detector arm.  Client software instantiates it once per arm and is
 * responsible for keeping the sample axis values synchronized
 * between the two instances (libhkl has no runtime linkage between
 * geometry instances anyway).
 *
 * NOTE: the generic q2/qper_qpar/tth2 engines are not registered
 * here because they hard-code detector axes named "gamma"/"delta"
 * (see hkl-pseudoaxis-common-q.c).
 */
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include "hkl-factory-private.h"        // for autodata_factories_, etc
#include "hkl-pseudoaxis-common-hkl-private.h"  // for hkl_mode_operations, etc
#include "hkl-pseudoaxis-common-readonly-private.h"

/**************/
/* Axes names */
/**************/

/* sample, outermost to innermost */
#define MU "mu"
#define OMEGA "omega"
#define CHI "chi"
#define OMEGA2 "omega2"
#define CHI2 "chi2"

/* detector arm */
#define TTH "tth"
#define PHI "phi"

/************/
/* Geometry */
/************/

/* TODO: check the exact rotation senses (signs) of every axis. */

#define HKL_GEOMETRY_NSLS_II_CDI_3_2_2_DESCRIPTION			\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 5 axes for the sample (outermost to innermost)\n"		\
        "\n\n"                                                          \
        "Note that the conventions in hkl are different than at NSLS-II\n"\
        "At NSLS-II the beam is in [0, 0, 1], vertical is [0, 1, 0] and outboard is [1, 0, 0]\n" \
        "\n\n" \
	"\n"								\
	"  + **" MU "** : rotating around the vertical :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **" OMEGA "** : rotating around the horizontal :math:`\\vec{y}` direction (0, 1, 0)\n" \
	"  + **" CHI "** : rotating around the beam :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"  + **" OMEGA2 "** : rotating around the horizontal :math:`\\vec{y}` direction (0, 1, 0)\n" \
	"  + **" CHI2 "** : rotating around the beam :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"\n"								\
	"+ 2 axes for the detector (one of the two physical arms)\n"	\
	"\n"								\
	"  + **" TTH "** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **" PHI "** : rotation around the :math:`\\vec{y}` direction (0, 1, 0)\n"

static const char* hkl_geometry_nsls_ii_cdi_3_2_2_axes[] = {MU, OMEGA, CHI, OMEGA2, CHI2, TTH, PHI};

static HklGeometry *hkl_geometry_new_nsls_ii_cdi_3_2_2(const HklFactory *factory)
{
	HklGeometry *self = hkl_geometry_new(factory, &hkl_geometry_operations_defaults);
	HklHolder *h;

	/* sample: mu carries omega carries chi carries omega2 carries chi2 */
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation(h, MU, 0, 0, 1, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, OMEGA, 0, 1, 0, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, CHI, 1, 0, 0, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, OMEGA2, 0, 1, 0, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, CHI2, 1, 0, 0, &hkl_unit_angle_deg);

	/* detector arm */
	h = hkl_geometry_add_holder(self);
	hkl_holder_add_rotation(h, TTH, 0, 0, 1, &hkl_unit_angle_deg);
	hkl_holder_add_rotation(h, PHI, 0, 1, 0, &hkl_unit_angle_deg);

	return self;
}

/*********/
/* Modes */
/*********/

/* skeleton hkl mode: move one sample axis and the two detector arm
 * axes.  RUBh_minus_Q_func solves for exactly three axes. */
static HklMode *lifting_detector_omega(void)
{
	static const char* axes_r[] = {MU, OMEGA, CHI, OMEGA2, CHI2, TTH, PHI};
	static const char* axes_w[] = {OMEGA, TTH, PHI};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		HKL_MODE_AUTO_INFO(__func__, axes_r, axes_w, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations,
				 TRUE);
}

/*****************/
/* mode readonly */
/*****************/

REGISTER_READONLY_INCIDENCE(hkl_engine_nsls_ii_cdi_3_2_2_incidence_new,
			    P99_PROTECT({MU, OMEGA, CHI, OMEGA2, CHI2}),
			    surface_parameters_y);

REGISTER_READONLY_EMERGENCE(hkl_engine_nsls_ii_cdi_3_2_2_emergence_new,
			    P99_PROTECT({MU, OMEGA, CHI, OMEGA2, CHI2, TTH, PHI}),
			    surface_parameters_y);

/***********/
/* Engines */
/***********/

static HklEngine *hkl_engine_nsls_ii_cdi_3_2_2_hkl_new(HklEngineList *engines)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new(engines);

	default_mode = lifting_detector_omega();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_mode_set(self, default_mode);

	return self;
}

/***************/
/* Engine list */
/***************/

static HklEngineList *hkl_engine_list_new_nsls_ii_cdi_3_2_2(const HklFactory *factory)
{
	HklEngineList *self = hkl_engine_list_new();

	hkl_engine_nsls_ii_cdi_3_2_2_hkl_new(self);
	hkl_engine_nsls_ii_cdi_3_2_2_incidence_new(self);
	hkl_engine_nsls_ii_cdi_3_2_2_emergence_new(self);

	return self;
}

/* Register the diffractometer into the factory */
REGISTER_DIFFRACTOMETER(nsls_ii_cdi_3_2_2, "CDI-3-2-2", HKL_GEOMETRY_NSLS_II_CDI_3_2_2_DESCRIPTION);
