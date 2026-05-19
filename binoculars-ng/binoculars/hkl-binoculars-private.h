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
 * Copyright (C) 2003-2026 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_BINOCULARS_PRIVATE_H__
#define __HKL_BINOCULARS_PRIVATE_H__

#include <cglm/struct.h>

#include "datatype99.h"

#include "hkl-binoculars.h"
#include "hkl-geometry-private.h"

#define SUCCEED 0

/********/
/* Axis */
/********/

struct _HklBinocularsAxis
{
	GQuark name; /* the name of the axis */
	size_t index; /* the index of the axis of the projection used */
	double resolution; /* the resolution of the bins */
	ptrdiff_t imin; /* the minimum index of the axis min = imin * resolution */
	ptrdiff_t imax; /* the maximum index of the axis max = imax * resolution */
};

typedef darray(HklBinocularsAxis) darray_axis;

extern void hkl_binoculars_axis_init_from_array(HklBinocularsAxis *self,

                                                const char *name,
                                                double *arr,
                                                size_t n_arr);

/*********/
/* Space */
/*********/

/* TODO how to avoid hardcoding the size of indexes_0 */
typedef struct _HklBinocularsSpaceItem HklBinocularsSpaceItem;
struct _HklBinocularsSpaceItem
{
        ptrdiff_t indexes_0[3]; /* for now hardcode the max number of axes */
        float intensity;
};

typedef darray(HklBinocularsSpaceItem) darray_HklBinocularsSpaceItem;


struct _HklBinocularsSpace
{
        darray_axis axes;
        size_t max_items;
        darray_HklBinocularsSpaceItem items;
};

/********/
/* Cube */
/********/

struct _HklBinocularsCube
{
        darray_axis axes;
        ptrdiff_t offset0;
	float *photons;
	unsigned int *contributions;
};

static inline size_t axis_size(const HklBinocularsAxis *self)
{
	return self->imax - self->imin + 1;
}

extern HklBinocularsCube *empty_cube_from_axes(const darray_axis *axes);

/************/
/* Geometry */
/************/

extern mat4s hkl_binoculars_parameter_transformation_get(const HklParameter *self);

extern mat4s hkl_binoculars_holder_transformation_get(const HklHolder *self);


#endif
