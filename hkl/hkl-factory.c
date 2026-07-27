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
 * Copyright (C) 2003-2019, 2026 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl-factory-private.h"
#include "hkl/ccan/array_size/array_size.h"

HklFactory **hkl_factory_get_all(size_t *n)
{
	/* ADD your diffractometer herre */

	extern HklFactory twoC;
	extern HklFactory eulerian4C_vertical;
	extern HklFactory soleil_mars;
	extern HklFactory eulerian4C_horizontal;
	extern HklFactory eulerian6C;
	extern HklFactory ESRF_BM28_PSIC;
	extern HklFactory PSIC;
	extern HklFactory kappa4C_vertical;
	extern HklFactory kappa6C;
	extern HklFactory soleil_sirius_kappa;
	extern HklFactory petra3_p09_eh2;
	extern HklFactory petra3_p23_4c;
	extern HklFactory petra3_p23_6c;
	extern HklFactory soleil_cristal_k4c;
	extern HklFactory soleil_nanoscopium_robot;
	extern HklFactory soleil_sirius_turret;
	extern HklFactory soleil_sixs_med_2_2;
	extern HklFactory soleil_sixs_med_1_2;
	extern HklFactory soleil_sixs_med_2_3;
	extern HklFactory soleil_sixs_med_2_3_v2;
	extern HklFactory zaxis;
	extern HklFactory aps_polar;
	extern HklFactory nsls_ii_cdi_3_2_2;

	static HklFactory *all[] = {
		&twoC,
		&eulerian4C_vertical,
		&soleil_mars,
		&eulerian4C_horizontal,
		&eulerian6C,
		&ESRF_BM28_PSIC,
		&PSIC,
		&kappa4C_vertical,
		&kappa6C,
		&soleil_sirius_kappa,
		&petra3_p09_eh2,
		&petra3_p23_4c,
		&petra3_p23_6c,
		&soleil_cristal_k4c,
		&soleil_nanoscopium_robot,
		&soleil_sirius_turret,
		&soleil_sixs_med_2_2,
		&soleil_sixs_med_1_2,
		&soleil_sixs_med_2_3,
		&soleil_sixs_med_2_3_v2,
		&zaxis,
		&aps_polar,
		&nsls_ii_cdi_3_2_2,
	};

	*n = ARRAY_SIZE(all);

	return all;
}

HklFactory *hkl_factory_get_by_name(const char *name, GError **error)
{
	size_t i, n;
	HklFactory **factories;

	factories = hkl_factory_get_all(&n);
	for(i=0;i<n; ++i)
		if (!strcmp(name, factories[i]->name))
			return factories[i];

	return NULL;
}

const char *hkl_factory_name_get(const HklFactory *self)
{
	return self->name;
}

HklGeometry *hkl_factory_create_new_geometry(const HklFactory *self)
{
	return self->create_new_geometry(self);
}

HklEngineList *hkl_factory_create_new_engine_list(const HklFactory *self)
{
	return self->create_new_engine_list(self);
}
