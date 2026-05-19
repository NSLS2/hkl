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

/* #define DEBUG */

#ifdef DEBUG
# define CGLM_DEFINE_PRINTS
# define CGLM_PRINT_PRECISION 7
#else
# define NDEBUG
#endif

#include <stdio.h>
#include <time.h>

#include "ccan/array_size/array_size.h"
#include "hkl-binoculars-private.h"
#include "hkl-matrix-private.h"
#include "hkl-quaternion-private.h"
#include "hkl-sample-private.h"
#include "hkl-vector-private.h"


/* TODO rename phi -> phix phiy phiz */

/* mark the masked pixels with this value */
#define MASKED PTRDIFF_MAX
#define REMOVED 0

/* Math */

static inline ptrdiff_t min(ptrdiff_t x, ptrdiff_t y)
{
	return y ^ ((x ^ y) & -(x < y));
}

static inline ptrdiff_t max(ptrdiff_t x, ptrdiff_t y)
{
	return x ^ ((x ^ y) & -(x < y));
}

/* Axis Limits */

datatype(
        AxisLimitType,
        (NoLimit),
        (Limit, ptrdiff_t)
        );

struct _HklBinocularsAxisLimits
{
        AxisLimitType imin;
        AxisLimitType imax;
};

void hkl_binoculars_axis_limits_free(HklBinocularsAxisLimits *self)
{
	free(self);
}

HklBinocularsAxisLimits *hkl_binoculars_axis_limits_new(const ptrdiff_t *imin,
                                                        const ptrdiff_t *imax)
{
	HklBinocularsAxisLimits *self = g_new(HklBinocularsAxisLimits, 1);

        if (NULL == imin){
                self->imin = NoLimit();
        } else {
                self->imin = Limit(*imin);
        }

        if (NULL == imax){
                self->imax = NoLimit();
        } else {
                self->imax = Limit(*imax);
        }

        return self;
}

/* Axis */

static inline double axis_min(const HklBinocularsAxis *self)
{
	return self->imin * self->resolution;
}

static inline double axis_max(const HklBinocularsAxis *self)
{
	return self->imax * self->resolution;
}

static inline void hkl_binoculars_axis_init(HklBinocularsAxis *self,
                                            const char *name,
                                            size_t index,
                                            ptrdiff_t imin,
                                            ptrdiff_t imax,
                                            double resolution)
{
	self->name = g_quark_from_string(name);
	self->index = index;
	self->resolution = resolution;
	self->imin = imin;
	self->imax = imax;
}

void hkl_binoculars_axis_init_from_array(HklBinocularsAxis *self,
                                         const char *name,
                                         double *arr,
                                         size_t n_arr)
{
        hkl_binoculars_axis_init(self, name, arr[0], arr[4], arr[5], arr[3]);
}

static inline int hkl_binoculars_axis_cmp(const HklBinocularsAxis *self,
                                          const HklBinocularsAxis *other)
{
        int res = 0;

        res |= self->name != other->name;
        res |= self->index != other->index;
        res |= self->resolution != other->resolution;
        res |= self->imin != other->imin;
        res |= self->imax != other->imax;

        return res;
}

double *hkl_binoculars_axis_array(const HklBinocularsAxis *self)
{
	double *arr = g_new0(double, 6);

        arr[0] = self->index;
        arr[1] = axis_min(self);
        arr[2] = axis_max(self);
        arr[3] = self->resolution;
        arr[4] = self->imin;
        arr[5] = self->imax;

	return arr;
}

/* check if *self contains *other */
static inline int hkl_binoculars_axis_contains_axis(const HklBinocularsAxis *self,
                                                    const HklBinocularsAxis *other)
{
        return self->imin <= other->imin && self->imax >= other->imax;
}

static inline void hkl_binoculars_axis_merge(HklBinocularsAxis *self, const HklBinocularsAxis *other)
{
	self->imin = min(self->imin, other->imin);
	self->imax = max(self->imax, other->imax);
}

void hkl_binoculars_axis_fprintf(FILE *f, const HklBinocularsAxis *self)
{
	fprintf(f, "%s : %ld min: %f(%ld) max: %f(%ld) res: %f size: %ld",
		g_quark_to_string(self->name),
                self->index,
		axis_min(self), self->imin,
                axis_max(self), self->imax,
		self->resolution, axis_size(self));
}


/* darray_axis */

static inline void merge_axes(darray_axis *axes,
                              const darray_axis *others)
{
        size_t i;

        for(i=0; i<darray_size(*axes); ++i)
                hkl_binoculars_axis_merge(&darray_item(*axes, i),
                                          &darray_item(*others, i));
}

static inline int does_not_include(const darray_axis *axes,
                                   const darray_axis *others)
{
        size_t i;
        int res = 0;

        if (darray_size(*axes) == darray_size(*others)){
                for(i=0; i<darray_size(*axes); ++i){
                        if (0 == hkl_binoculars_axis_contains_axis(&darray_item(*axes, i),
                                                                   &darray_item(*others, i))){
                                res = 1;
                                break;
                        }
                }
        } else {
                res = 1;
        }

        return res;
}


/* Space */

static inline void hkl_binoculars_space_item_fprintf(FILE *f, const HklBinocularsSpaceItem *self)
{
        fprintf(f, "item->indexes(%p) v: %ld %ld %ld, intensity: %g",
                &self->indexes_0[0],
                self->indexes_0[0], self->indexes_0[1], self->indexes_0[2],
                self->intensity);
}

static inline int space_is_empty(const HklBinocularsSpace *space)
{
        return 0 == darray_size(space->items);
}

static inline void space_update_axes(HklBinocularsSpace *space,
                                     const char *names[],
                                     size_t n_pixels,
                                     const double resolutions[])
{
        size_t i;
        HklBinocularsSpaceItem *item;
        HklBinocularsSpaceItem minimum;
        HklBinocularsSpaceItem maximum;

        if (space_is_empty(space))
                return;

        minimum = maximum = darray_item(space->items, 0);

        for(i=0; i<darray_size(space->axes); ++i){
                darray_foreach(item, space->items){
                        minimum.indexes_0[i] = min(minimum.indexes_0[i], item->indexes_0[i]);
                        maximum.indexes_0[i] = max(maximum.indexes_0[i], item->indexes_0[i]);
                }
        }

        for(i=0; i<darray_size(space->axes); ++i){
                HklBinocularsAxis *axis = &darray_item(space->axes, i);
                hkl_binoculars_axis_init(axis, names[i], i,
                                         minimum.indexes_0[i], maximum.indexes_0[i],
                                         resolutions[i]);
        }
}

static inline int item_in_the_limits(const HklBinocularsSpaceItem *item,
                                     const HklBinocularsAxisLimits **limits,
                                     size_t n_limits)
{
        int res = TRUE;

        if (NULL != limits){
                for(size_t i=0; i<n_limits; ++i){
                        ptrdiff_t v = item->indexes_0[i];

                        match(limits[i]->imin){
                                of(NoLimit){
                                }
                                of(Limit, imin){
                                        if (v < *imin) res = FALSE;
                                        break;
                                }
                        }

                        match(limits[i]->imax){
                                of(NoLimit){
                                }
                                of(Limit, imax){
                                        if(v > *imax) res = FALSE;
                                        break;
                                }
                        }
                }
        }

        return res;
}

HklBinocularsSpace *hkl_binoculars_space_new(size_t max_items, size_t n_axes)
{
	HklBinocularsSpace *self = g_new(HklBinocularsSpace, 1);

        self->max_items = max_items;
        darray_init(self->items);
        darray_resize(self->items, max_items);
        darray_init(self->axes);
        darray_resize(self->axes, n_axes);

	return self;
}

void hkl_binoculars_space_free(HklBinocularsSpace *self)
{
	darray_free(self->axes);
        darray_free(self->items);
	free(self);
}

void hkl_binoculars_space_fprintf(FILE *f, const HklBinocularsSpace *self)
{
        size_t masked;
        HklBinocularsAxis *axis;

	fprintf(f, "\nHklBinocularsSpace: %p", self);
	fprintf(f, "\nn_indexes_0: %ld", darray_size(self->items));
        if(space_is_empty(self)){
                fprintf(f, "\nempty");
        } else {
                fprintf(f, "\nn_axes: %ld", darray_size(self->axes));
                darray_foreach(axis, self->axes){
                        fprintf(f, "\n");
                        hkl_binoculars_axis_fprintf(f, axis);
                }

                masked = self->max_items - darray_size(self->items);
                fprintf(f, "\nmasked pixels: %ld (%f%%)", masked, (double)masked / self->max_items * 100);
        }
}

/* angles */

#define HKL_BINOCULARS_SPACE_ANGLES_IMPL(image_t)                       \
        HKL_BINOCULARS_SPACE_ANGLES_DECL(image_t)                       \
        {                                                               \
                size_t i, j;                                            \
                const char * names[] = {"delta_lab", "gamma_lab", "tth"}; \
                double delta0, gamma0, tth;                             \
                                                                        \
                assert(ARRAY_SIZE(names) == darray_size(space->axes));  \
                assert(ARRAY_SIZE(names) == n_resolutions);             \
                assert(n_pixels == space->max_items);                   \
                                                                        \
                darray_size(space->items) = 0;                          \
                                                                        \
                const double *p_x = &pixels_coordinates[0 * n_pixels];  \
                const double *p_y = &pixels_coordinates[1 * n_pixels];  \
                const double *p_z = &pixels_coordinates[2 * n_pixels];  \
                                                                        \
                HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D); \
                const HklQuaternion q = hkl_geometry_detector_rotation_get(geometry, detector); \
                                                                        \
                for(i=0;i<n_pixels;++i){                                \
                        if(NULL == masked || 0 == masked[i]){           \
                                HklBinocularsSpaceItem item;            \
                                HklVector v = {{p_x[i], p_y[i], p_z[i]}}; \
                                                                        \
                                hkl_vector_rotated_quaternion(&v, &q);  \
                                delta0 = atan2(v.data[2], v.data[0]);   \
                                gamma0 = M_PI_2 - atan2(sqrt(v.data[2] * v.data[2] + v.data[0] * v.data[0]), v.data[1]); \
                                tth = acos(v.data[0]);                  \
                                                                        \
                                v.data[0] = delta0 / M_PI * 180.0;      \
                                v.data[1] = gamma0 / M_PI * 180.0;      \
                                v.data[2] = tth / M_PI * 180.0;         \
                                                                        \
                                for(j=0; j<ARRAY_SIZE(names); ++j){     \
                                        item.indexes_0[j] = rint(v.data[j] / resolutions[j]); \
                                }                                       \
                                item.intensity = rint((double)image[i] * weight / flat[i]); \
                                                                        \
                                                                        \
                                if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                        darray_append(space->items, item); \
                        }                                               \
                }                                                       \
                                                                        \
                space_update_axes(space, names, n_pixels, resolutions); \
                                                                        \
                hkl_detector_free(detector);                            \
        }

HKL_BINOCULARS_SPACE_ANGLES_IMPL(double);
HKL_BINOCULARS_SPACE_ANGLES_IMPL(int32_t);
HKL_BINOCULARS_SPACE_ANGLES_IMPL(uint16_t);
HKL_BINOCULARS_SPACE_ANGLES_IMPL(uint32_t);

/* qcustom */

struct _HklBinocularsProjectionAxis
{
        const char *name;
        const char *description;
};

typedef struct _HklBinocularsProjectionAxis HklBinocularsProjectionAxis;

static const HklBinocularsProjectionAxis azimuth =
{
        .name = "azimuth",
        .description = "The azimuthal angle in the yz plan of the kf vector",
};
static const HklBinocularsProjectionAxis thetax =
{
        .name = "thetax",
        .description = "The angle beetween q and the qx axis",
};
static const HklBinocularsProjectionAxis thetay =
{
        .name = "thetay",
        .description = "The angle beetween q and the qy axis",
};
static const HklBinocularsProjectionAxis thetaz =
{
        .name = "thetaz",
        .description = "The angle beetween q and the qz axis",
};
static const HklBinocularsProjectionAxis deltalab =
{
        .name = "deltalab",
        .description = "The delta horizontal deviation in the lab basis",
};
static const HklBinocularsProjectionAxis gammalab =
{
        .name = "gammalab",
        .description = "The gamma elevation in the lab basis",
};
static const HklBinocularsProjectionAxis q =
{
        .name = "q",
        .description = "The norm of the q vector",
};
static const HklBinocularsProjectionAxis qpar =
{
        .name = "qpar",
        .description = "The norm of the q vector projected onto the plan parallel to the surface."
};
static const HklBinocularsProjectionAxis qpars =
{
        .name = "qpars",
        .description = "The signed norm of the q vector projected onto the plan parallel to the surface."
};
static const HklBinocularsProjectionAxis qper =
{
        .name = "qper",
        .description = "The norm of the q vector projected onto the surface vector (perpendicular to the surface)."
};
static const HklBinocularsProjectionAxis qx =
{
        .name = "qx",
        .description = "The x coordinate of the Q vector in the sample basis."
};
static const HklBinocularsProjectionAxis qy =
{
        .name = "qy",
        .description = "The y coordinate of the Q vector in the sample basis."
};
static const HklBinocularsProjectionAxis qz =
{
        .name = "qz",
        .description = "The z coordinate of the Q vector in the sample basis."
};
static const HklBinocularsProjectionAxis phix =
{
        .name = "phix",
        .description = "The azimuthal angle in the yz plan of the q vector",
};
static const HklBinocularsProjectionAxis phiy =
{
        .name = "phiy",
        .description = "The azimuthal angle in the xz plan of the q vector",
};
static const HklBinocularsProjectionAxis phiz =
{
        .name = "phiz",
        .description = "The azimuthal angle in the xy plan of the q vector",
};
static const HklBinocularsProjectionAxis sampleaxis =
{
        .name = "sampleaxis",
        .description = "The name of a sample axis",
};
static const HklBinocularsProjectionAxis scannumber =
{
        .name = "scannumber",
        .description = "The scan number of the scan",
};
static const HklBinocularsProjectionAxis timestamp =
{
        .name = "timestamp",
        .description = "The timestamp of each image",
};
static const HklBinocularsProjectionAxis timescan0 =
{
        .name = "timescan0",
        .description = "The timestamp of the first image of a scan",
};
static const HklBinocularsProjectionAxis tth =
{
        .name = "tth",
        .description = "The 2 * theta angle.",
};
static const HklBinocularsProjectionAxis xp =
{
        .name = "xp",
        .description = "The x coordinate of pixels projected in the yz plan",
};
static const HklBinocularsProjectionAxis yp =
{
        .name="yp",
        .description = "The y coordinate of pixels projected in the yz plan",
};
static const HklBinocularsProjectionAxis x =
{
        .name="x",
        .description = "The x coordinate of pixels",
};
static const HklBinocularsProjectionAxis y =
{
        .name="y",
        .description = "The y coordinate of pixels",
};
static const HklBinocularsProjectionAxis z =
{
        .name="z",
        .description = "The z coordinate of pixels",
};


#define PROJECTION(...) do {                                            \
                int i;                                                  \
                int n;                                                  \
                static const HklBinocularsProjectionAxis axes[] = {__VA_ARGS__}; \
                static const char *axes_names[ARRAY_SIZE(axes)];        \
                                                                        \
                n = ARRAY_SIZE(axes);                                   \
                assert(n <= darray_size(space->axes));                  \
                assert(n <= n_resolutions);                             \
                                                                        \
                for(i=0; i<n; ++i)                                      \
                        axes_names[i] = axes[i].name;                   \
                names = axes_names;                                     \
        }while(0)

static const char **axis_name_from_subprojection(HklBinocularsQCustomSubProjectionEnum subprojection,
                                                 HklBinocularsSpace *space,
                                                 int n_resolutions)
{
        const char **names = NULL;

        switch(subprojection){
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_QZ:
        case HKL_BINOCULARS_QCUSTOM_NUM_SUBPROJECTIONS:
        {
                PROJECTION(qx, qy, qz);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TTH_TIMESTAMP:
        {
                PROJECTION(q, tth, timestamp);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TIMESTAMP:
        {
                PROJECTION(q, timestamp);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_TIMESTAMP:
        {
                PROJECTION(qpar, qper, timestamp);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER:
        {
                PROJECTION(qpar, qper);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIX_QX:
        {
                PROJECTION(q, phix, qx);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIY_QY:
        {
                PROJECTION(q, phiy, qy);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIZ_QZ:
        {
                PROJECTION(q, phiz, qz);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_STEREO:
        {
                PROJECTION(q, xp, yp);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_DELTALAB_GAMMALAB_SAMPLEAXIS:
        {
                PROJECTION(deltalab, gammalab, sampleaxis);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_X_Y_Z:
        {
                PROJECTION(x, y, z);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Y_Z_TIMESTAMP:
        {
                PROJECTION(y, z, timestamp);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_QPAR_QPER:
        {
                PROJECTION(q, qpar, qper);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPARS_QPER_TIMESTAMP:
        {
                PROJECTION(qpars, qper, timestamp);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_SAMPLEAXIS:
        {
                PROJECTION(qpar, qper, sampleaxis);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SAMPLEAXIS_TTH:
        {
                PROJECTION(q, sampleaxis, tth);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SAMPLEAXIS_TIMESTAMP:
        {
                PROJECTION(q, sampleaxis, timestamp);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_TIMESTAMP:
        {
                PROJECTION(qx, qy, timestamp);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QZ_TIMESTAMP:
        {
                PROJECTION(qx, qz, timestamp);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QY_QZ_TIMESTAMP:
        {
                PROJECTION(qy, qz, timestamp);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_TTH_AZIMUTH:
        {
                PROJECTION(tth, azimuth);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TIMESCAN0:
        {
                PROJECTION(q, timescan0);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SCANNUMBER:
        {
                PROJECTION(q,  scannumber);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_TTH_SCANNUMBER:
        {
                PROJECTION(q, scannumber);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIX_Q_THETAX:
        {
                PROJECTION(phix, q, thetax);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIY_Q_THETAY:
        {
                PROJECTION(phiy, q, thetay);
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIZ_Q_THETAZ:
        {
                PROJECTION(phiz, q, thetaz);
                break;
        }
        }

	if (NULL == names)
		PROJECTION(qx, qy, qz);

        return names;
}

static inline int not_masked(const uint8_t *masked, size_t idx, double img, double *image_threshold)
{
        return (NULL == masked || 0 == masked[idx]) && (NULL == image_threshold || img < *image_threshold);
}

static inline double polarisation(vec3s kf, double weight, int do_polarisation)
{
        if (do_polarisation){
                CGLM_ALIGN_MAT vec3s epsilon = {{0, 1, 0}};
		/* 1 - cos2 theta */
                float d = glms_vec3_dot(epsilon, kf);
		float cos2 = d * d / glms_vec3_norm2(kf);
                weight = weight / (1 - cos2);
        }
        return weight;
}

static inline double compute_q(vec3s q)
{
        return glms_vec3_norm(q);
}

static inline double compute_qx(vec3s q)
{
        return q.raw[0];
}

static inline double compute_qy(vec3s q)
{
        return q.raw[1];
}

static inline double compute_qz(vec3s q)
{
        return q.raw[2];
}

static inline double compute_qpar(vec3s q)
{
        return sqrt(q.raw[0] * q.raw[0] + q.raw[1] * q.raw[1]);
}

static inline double compute_qpar_signed(vec3s q)
{
        float qpar = compute_qpar(q);

        if(q.raw[1] != 0.0){
                if(signbit(q.raw[1]) != 0){
                        qpar = -qpar;
                }
        }else{
                if(signbit(q.raw[0]) != 0){
                        qpar = -qpar;
                }
        }
        return qpar;
}

static inline double compute_qper(vec3s q)
{
        return compute_qz(q);
}

static inline double compute_tth(float q, float k)
{
        return asin(q / 2 / k) * 2 / M_PI * 180;
}

static inline double compute_azimuth(vec3s kf)
{
        return (atan2(kf.raw[2], kf.raw[1])) / M_PI * 180;
}

#define HKL_BINOCULARS_SPACE_QCUSTOM_IMPL(image_t)			\
        HKL_BINOCULARS_SPACE_QCUSTOM_DECL(image_t)			\
        {                                                               \
		size_t i;						\
                HklBinocularsSpaceItem item;                            \
                double correction;                                      \
		const char **names = axis_name_from_subprojection(subprojection, space, n_resolutions); \
		assert(n_pixels == space->max_items);			\
                                                                        \
		const double *q_x = &pixels_coordinates[0 * n_pixels];	\
		const double *q_y = &pixels_coordinates[1 * n_pixels];	\
		const double *q_z = &pixels_coordinates[2 * n_pixels];	\
                                                                        \
		HklSample *sample = hkl_sample_new("test");		\
		HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D); \
		HklHolder *holder_d = hkl_geometry_detector_holder_get(geometry, detector); \
                CGLM_ALIGN_MAT mat4s m_holder_d = hkl_binoculars_holder_transformation_get(holder_d); \
		const HklVector ki_v = hkl_geometry_ki_get(geometry);	\
                CGLM_ALIGN_MAT vec3s ki = {{ki_v.data[0], ki_v.data[1], ki_v.data[2]}}; \
	        float k = glms_vec3_norm(ki);                           \
		HklHolder *holder_s = hkl_geometry_sample_holder_get(geometry,sample); \
                CGLM_ALIGN_MAT mat4s m_holder_s = hkl_binoculars_holder_transformation_get(holder_s); \
                                                                        \
		switch(surf){						\
		case HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL:	\
		{							\
                        CGLM_ALIGN_MAT vec3s axis = GLMS_XUP;           \
			CGLM_ALIGN_MAT mat4s m_q_ub = glms_rotate_make(-M_PI_2, axis); \
                                                                        \
                        m_holder_s = glms_mat4_mul(m_holder_s, m_q_ub); \
			break;						\
		}							\
		case HKL_BINOCULARS_SURFACE_ORIENTATION_HORIZONTAL:	\
		case HKL_BINOCULARS_SURFACE_ORIENTATION_NUM_ORIENTATION: \
			break;						\
		}							\
                                                                        \
                CGLM_ALIGN_MAT vec3s euler_xyz = {{uqx, uqy, uqz}};     \
                m_holder_s = glms_mat4_mul(m_holder_s, glms_euler_xyz(euler_xyz)); \
                                                                        \
		m_holder_s = glms_mat4_inv(m_holder_s);                 \
                                                                        \
		darray_size(space->items) = 0;				\
                                                                        \
                glms_mat4_print(m_holder_s, stdout);                    \
                glms_mat4_print(m_holder_d, stdout);                    \
                                                                        \
                switch(subprojection){                                  \
                case HKL_BINOCULARS_QCUSTOM_NUM_SUBPROJECTIONS:         \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_QZ:    \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v, ki);       \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        item.indexes_0[0] = rint(v.raw[0] / resolutions[0]); \
                                        item.indexes_0[1] = rint(v.raw[1] / resolutions[1]); \
                                        item.indexes_0[2] = rint(v.raw[2] / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TTH_TIMESTAMP: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
                                        float tth = compute_tth(q, k);  \
					item.indexes_0[0] = rint(q / resolutions[0]); \
					item.indexes_0[1] = rint(tth / resolutions[1]); \
					item.indexes_0[2] = rint(timestamp / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TIMESTAMP: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
					item.indexes_0[0] = rint(q / resolutions[0]); \
					item.indexes_0[1] = rint(timestamp / resolutions[1]); \
					item.indexes_0[2] = REMOVED;	\
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_TIMESTAMP: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float qpar = compute_qpar(v);   \
                                        float qper = compute_qper(v);   \
					item.indexes_0[0] = rint(qpar / resolutions[0]); \
                                        item.indexes_0[1] = rint(qper / resolutions[1]); \
					item.indexes_0[2] = rint(timestamp / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER:   \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float qpar = compute_qpar(v);   \
                                        float qper = compute_qper(v);   \
					item.indexes_0[0] = rint(qpar / resolutions[0]); \
                                        item.indexes_0[1] = rint(qper / resolutions[1]); \
					item.indexes_0[2] = REMOVED;	\
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIX_QX:	\
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
					item.indexes_0[0] = rint(q / resolutions[0]); \
					item.indexes_0[1] = rint((atan2(v.raw[2], -v.raw[1])) / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = rint(v.raw[0] / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIY_QY:	\
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
					item.indexes_0[0] = rint(q / resolutions[0]); \
					item.indexes_0[1] = rint((atan2(v.raw[2], v.raw[0])) / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = rint(v.raw[1] / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIZ_QZ:	\
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
					item.indexes_0[0] = rint(q / resolutions[0]); \
					item.indexes_0[1] = rint((atan2(v.raw[1], v.raw[0])) / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = rint(v.raw[2] / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_STEREO:    \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
                                        if (v.raw[2] < 0.0) continue;   \
					double ratio = v.raw[2] + q;	\
					item.indexes_0[0] = rint(q / resolutions[0]); \
					item.indexes_0[1] = rint(v.raw[0] / ratio / resolutions[1]); \
					item.indexes_0[2] = rint(v.raw[1] / ratio / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_DELTALAB_GAMMALAB_SAMPLEAXIS: \
                {                                                       \
                        const HklParameter *p = hkl_geometry_axis_get(geometry, sample_axis, NULL); \
                        if (NULL != p){                                 \
                                int axis = rint(hkl_parameter_value_get(p, HKL_UNIT_USER) / resolutions[2]); \
                                                                        \
                                for(i=0;i<n_pixels;++i){                \
					if(not_masked(masked, i, image[i], image_threshold)){ \
                                                CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                                v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                                correction = polarisation(v, weight, do_polarisation_correction); \
                                                                        \
                                                item.indexes_0[0] = rint(atan2(v.raw[2], sqrt(v.raw[0] * v.raw[0] + v.raw[1] * v.raw[1])) / M_PI * 180 / resolutions[0]); \
                                                item.indexes_0[1] = rint(atan2(v.raw[1], v.raw[0]) / M_PI * 180 / resolutions[1]); \
                                                item.indexes_0[2] = axis; \
                                                item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                                if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                        darray_append(space->items, item); \
                                        }                               \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_X_Y_Z:       \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        glms_vec3_print(v, stdout);     \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        glms_mat4_print(m_holder_d, stdout); \
                                        glms_vec3_print(v, stdout);     \
                                                                        \
					item.indexes_0[0] = rint(v.raw[0] / resolutions[0]); \
					item.indexes_0[1] = rint(v.raw[1] / resolutions[1]); \
					item.indexes_0[2] = rint(v.raw[2] / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Y_Z_TIMESTAMP: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                                                        \
					item.indexes_0[0] = rint(v.raw[1] / resolutions[0]); \
					item.indexes_0[1] = rint(v.raw[2] / resolutions[1]); \
					item.indexes_0[2] = rint(timestamp / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_QPAR_QPER:	\
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
                                        float qpar = compute_qpar(v);	\
                                        float qper = compute_qper(v);   \
					item.indexes_0[0] = rint(q / resolutions[0]); \
					item.indexes_0[1] = rint(qpar / resolutions[1]); \
					item.indexes_0[2] = rint(qper / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPARS_QPER_TIMESTAMP: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float qpars = compute_qpar_signed(v); \
                                        float qper = compute_qper(v);   \
					item.indexes_0[0] = rint(qpars / resolutions[0]); \
                                        item.indexes_0[1] = rint(qper / resolutions[1]); \
					item.indexes_0[2] = rint(timestamp / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_SAMPLEAXIS: \
                {                                                       \
                        const HklParameter *p = hkl_geometry_axis_get(geometry, sample_axis, NULL); \
                        if (NULL != p){                                 \
                                int axis = rint(hkl_parameter_value_get(p, HKL_UNIT_USER) / resolutions[2]); \
                                                                        \
                                for(i=0;i<n_pixels;++i){                \
					if(not_masked(masked, i, image[i], image_threshold)){ \
                                                CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                                v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                                v = glms_vec3_scale_as(v, k); \
                                                correction = polarisation(v, weight, do_polarisation_correction); \
                                                v = glms_vec3_sub(v , ki); \
                                                v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                                float qpar = compute_qpar(v); \
                                                float qper = compute_qper(v); \
                                                item.indexes_0[0] = rint(qpar / resolutions[0]); \
                                                item.indexes_0[1] = rint(qper / resolutions[1]); \
                                                item.indexes_0[2] = axis; \
                                                item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                                if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                        darray_append(space->items, item); \
                                        }                               \
                                }                                       \
                                break;                                  \
                        }                                               \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SAMPLEAXIS_TTH: \
                {                                                       \
                        const HklParameter *p = hkl_geometry_axis_get(geometry, sample_axis, NULL); \
                        if (NULL != p){                                 \
                                int axis = rint(hkl_parameter_value_get(p, HKL_UNIT_USER) / resolutions[1]); \
                                                                        \
                                for(i=0;i<n_pixels;++i){                \
					if(not_masked(masked, i, image[i], image_threshold)){ \
                                                CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                                v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                                v = glms_vec3_scale_as(v, k); \
                                                correction = polarisation(v, weight, do_polarisation_correction); \
                                                v = glms_vec3_sub(v , ki); \
                                                v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                                float q = compute_q(v); \
                                                double tth = compute_tth(q, k); \
                                                item.indexes_0[0] = rint(q / resolutions[0]); \
                                                item.indexes_0[1] = axis; \
                                                item.indexes_0[2] = rint(tth / resolutions[2]); \
                                                item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                                if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                        darray_append(space->items, item); \
                                        }                               \
                                }                                       \
                                break;                                  \
                        }                                               \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SAMPLEAXIS_TIMESTAMP: \
                {                                                       \
                        const HklParameter *p = hkl_geometry_axis_get(geometry, sample_axis, NULL); \
                        if (NULL != p){                                 \
                                int axis = rint(hkl_parameter_value_get(p, HKL_UNIT_USER) / resolutions[1]); \
                                                                        \
                                for(i=0;i<n_pixels;++i){                \
					if(not_masked(masked, i, image[i], image_threshold)){ \
                                                CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                                v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                                v = glms_vec3_scale_as(v, k); \
                                                correction = polarisation(v, weight, do_polarisation_correction); \
                                                v = glms_vec3_sub(v , ki); \
                                                v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                                float q = compute_q(v); \
                                                item.indexes_0[0] = rint(q / resolutions[0]); \
                                                item.indexes_0[1] = axis; \
                                                item.indexes_0[2] = rint(timestamp / resolutions[2]); \
                                                item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                                if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                        darray_append(space->items, item); \
                                        }                               \
                                }                                       \
                                break;                                  \
                        }                                               \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_TIMESTAMP: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v, ki);       \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        item.indexes_0[0] = rint(v.raw[0] / resolutions[0]); \
                                        item.indexes_0[1] = rint(v.raw[1] / resolutions[1]); \
                                        item.indexes_0[2] = rint(timestamp / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QZ_TIMESTAMP: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v, ki);       \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        item.indexes_0[0] = rint(v.raw[0] / resolutions[0]); \
                                        item.indexes_0[1] = rint(v.raw[2] / resolutions[1]); \
                                        item.indexes_0[2] = rint(timestamp / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QY_QZ_TIMESTAMP: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v, ki);       \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        item.indexes_0[0] = rint(v.raw[1] / resolutions[0]); \
                                        item.indexes_0[1] = rint(v.raw[2] / resolutions[1]); \
                                        item.indexes_0[2] = rint(timestamp / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_TTH_AZIMUTH: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        double azimuth = compute_azimuth(v); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
                                        double tth = compute_tth(q, k); \
					item.indexes_0[0] = rint(tth / resolutions[0]); \
					item.indexes_0[1] = rint(azimuth / resolutions[1]); \
					item.indexes_0[2] = REMOVED;    \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TIMESCAN0:	\
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
					item.indexes_0[0] = rint(q / resolutions[0]); \
					item.indexes_0[1] = rint(timescan0 / resolutions[1]); \
					item.indexes_0[2] = REMOVED;	\
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SCANNUMBER: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
					item.indexes_0[0] = rint(q / resolutions[0]); \
					item.indexes_0[1] = rint(scannumber / resolutions[1]); \
					item.indexes_0[2] = REMOVED;	\
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_TTH_SCANNUMBER: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
                                        double tth = compute_tth(q, k); \
					item.indexes_0[0] = rint(tth / resolutions[0]); \
					item.indexes_0[1] = rint(scannumber / resolutions[1]); \
					item.indexes_0[2] = REMOVED;	\
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIX_Q_THETAX: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
					item.indexes_0[0] = rint((atan2(v.raw[2], -v.raw[1])) / M_PI * 180 / resolutions[0]); \
					item.indexes_0[1] = rint(q / resolutions[1]); \
					item.indexes_0[2] = rint((acos(v.raw[0] / q) / M_PI * 180) / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIY_Q_THETAY: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
					item.indexes_0[0] = rint((atan2(v.raw[2], v.raw[0])) / M_PI * 180 / resolutions[0]); \
					item.indexes_0[1] = rint(q / resolutions[1]); \
					item.indexes_0[2] = rint((acos(v.raw[1] / q) / M_PI * 180) / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIZ_Q_THETAZ: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i, image[i], image_threshold)){ \
                                        CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        v = glms_mat4_mulv3(m_holder_d, v, 1); \
                                        v = glms_vec3_scale_as(v, k);   \
                                        correction = polarisation(v, weight, do_polarisation_correction); \
                                        v = glms_vec3_sub(v , ki);      \
                                        v = glms_mat4_mulv3(m_holder_s, v, 0); \
                                                                        \
                                        float q = compute_q(v);         \
					item.indexes_0[0] = rint((atan2(v.raw[1], v.raw[0])) / M_PI * 180 / resolutions[0]); \
					item.indexes_0[1] = rint(q / resolutions[1]); \
					item.indexes_0[2] = rint((acos(v.raw[2] / q) / M_PI * 180) / resolutions[2]); \
                                        item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                }                                                       \
		space_update_axes(space, names, n_pixels, resolutions);	\
		hkl_detector_free(detector);				\
		hkl_sample_free(sample);				\
        }

HKL_BINOCULARS_SPACE_QCUSTOM_IMPL(double);
HKL_BINOCULARS_SPACE_QCUSTOM_IMPL(int32_t);
HKL_BINOCULARS_SPACE_QCUSTOM_IMPL(uint16_t);
HKL_BINOCULARS_SPACE_QCUSTOM_IMPL(uint32_t);

/* hkl */

#define HKL_BINOCULARS_SPACE_HKL_IMPL(image_t)                          \
        HKL_BINOCULARS_SPACE_HKL_DECL(image_t)                          \
        {                                                               \
                size_t i;                                               \
                double correction;                                      \
                const char * names[] = {"H", "K", "L"};                 \
                HklBinocularsSpaceItem item;                            \
                                                                        \
                assert(ARRAY_SIZE(names) == darray_size(space->axes));  \
                assert(ARRAY_SIZE(names) == n_resolutions);             \
                assert(n_pixels == space->max_items);                   \
                                                                        \
                const double *h = &pixels_coordinates[0 * n_pixels];    \
                const double *k = &pixels_coordinates[1 * n_pixels];    \
                const double *l = &pixels_coordinates[2 * n_pixels];    \
                                                                        \
                HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D); \
                HklHolder *holder_d = hkl_geometry_detector_holder_get(geometry, detector); \
                CGLM_ALIGN_MAT mat4s m_holder_d = hkl_binoculars_holder_transformation_get(holder_d); \
                const HklVector ki_v = hkl_geometry_ki_get(geometry);	\
                CGLM_ALIGN_MAT vec3s ki = {{ki_v.data[0], ki_v.data[1], ki_v.data[2]}}; \
                float K = glms_vec3_norm(ki);                           \
                HklHolder *holder_s = hkl_geometry_sample_holder_get(geometry,sample); \
                CGLM_ALIGN_MAT mat4s m_holder_s = hkl_binoculars_holder_transformation_get(holder_s); \
                                                                        \
                const HklMatrix *UB = hkl_sample_UB_get(sample);        \
                CGLM_ALIGN_MAT mat4s ub = {{{UB->data[0][0], UB->data[1][0], UB->data[2][0], 0}, \
                                            {UB->data[0][1], UB->data[1][1], UB->data[2][1], 0}, \
                                            {UB->data[0][2], UB->data[1][2], UB->data[2][2], 0}, \
                                            {0, 0, 0, 1}}};             \
                glms_mat4_print(ub, stdout);                            \
                m_holder_s = glms_mat4_mul(m_holder_s, ub);             \
                glms_mat4_print(m_holder_s, stdout);                    \
                m_holder_s = glms_mat4_inv(m_holder_s);                 \
                                                                        \
                darray_size(space->items) = 0;                          \
                                                                        \
                glms_mat4_print(m_holder_s, stdout);                    \
                glms_mat4_print(m_holder_d, stdout);                    \
                                                                        \
                for(i=0;i<n_pixels;++i){                                \
			if(not_masked(masked, i, image[i], image_threshold)){ \
                                CGLM_ALIGN_MAT vec3s v = {{h[i], k[i], l[i]}}; \
                                                                        \
                                v = glms_mat4_mulv3(m_holder_d, v, 1);  \
                                v = glms_vec3_scale_as(v, K);           \
                                correction = polarisation(v, weight, do_polarisation_correction); \
                                v = glms_vec3_sub(v, ki);               \
                                v = glms_mat4_mulv3(m_holder_s, v, 0);  \
                                                                        \
                                item.indexes_0[0] = rint(v.raw[0] / resolutions[0]); \
                                item.indexes_0[1] = rint(v.raw[1] / resolutions[1]); \
                                item.indexes_0[2] = rint(v.raw[2] / resolutions[2]); \
                                item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                        darray_append(space->items, item); \
                        }                                               \
                }                                                       \
                                                                        \
                space_update_axes(space, names, n_pixels, resolutions); \
                                                                        \
                hkl_detector_free(detector);                            \
        }

HKL_BINOCULARS_SPACE_HKL_IMPL(double);
HKL_BINOCULARS_SPACE_HKL_IMPL(int32_t);
HKL_BINOCULARS_SPACE_HKL_IMPL(uint16_t);
HKL_BINOCULARS_SPACE_HKL_IMPL(uint32_t);

/* test */

#define HKL_BINOCULARS_SPACE_TEST_IMPL(image_t)                         \
        HKL_BINOCULARS_SPACE_TEST_DECL(image_t)                         \
        {                                                               \
                size_t i;                                               \
                const char * names[] = {"H", "K", "L"};                 \
                double correction;                                      \
                HklBinocularsSpaceItem item;                            \
                                                                        \
                assert(ARRAY_SIZE(names) == darray_size(space->axes));  \
                assert(ARRAY_SIZE(names) == n_resolutions);             \
                assert(n_pixels == space->max_items);                   \
                                                                        \
                const double *h = &pixels_coordinates[0 * n_pixels];    \
                const double *k = &pixels_coordinates[1 * n_pixels];    \
                const double *l = &pixels_coordinates[2 * n_pixels];    \
                                                                        \
                HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D); \
                HklHolder *holder_d = hkl_geometry_detector_holder_get(geometry, detector); \
                CGLM_ALIGN_MAT mat4s m_holder_d = hkl_binoculars_holder_transformation_get(holder_d); \
                const HklVector ki_v = hkl_geometry_ki_get(geometry);	\
                CGLM_ALIGN_MAT vec3s ki = {{ki_v.data[0], ki_v.data[1], ki_v.data[2]}}; \
                float K = glms_vec3_norm(ki);                           \
                HklHolder *holder_s = hkl_geometry_sample_holder_get(geometry,sample); \
                CGLM_ALIGN_MAT mat4s m_holder_s = hkl_binoculars_holder_transformation_get(holder_s); \
                                                                        \
                const HklMatrix *UB = hkl_sample_UB_get(sample);        \
                CGLM_ALIGN_MAT mat4s ub = {{{UB->data[0][0], UB->data[1][0], UB->data[2][0], 0}, \
                                            {UB->data[0][1], UB->data[1][1], UB->data[2][1], 0}, \
                                            {UB->data[0][2], UB->data[1][2], UB->data[2][2], 0}, \
                                            {0, 0, 0, 1}}};             \
                glms_mat4_print(ub, stdout);                            \
                m_holder_s = glms_mat4_mul(m_holder_s, ub);             \
                glms_mat4_print(m_holder_s, stdout);                    \
                m_holder_s = glms_mat4_inv(m_holder_s);                 \
                                                                        \
                darray_size(space->items) = 0;                          \
                                                                        \
                glms_mat4_print(m_holder_s, stdout);                    \
                glms_mat4_print(m_holder_d, stdout);                    \
                                                                        \
                for(i=0;i<n_pixels;++i){                                \
			if(not_masked(masked, i, image[i], image_threshold)){ \
                                CGLM_ALIGN_MAT vec3s v = {{h[i], k[i], l[i]}}; \
                                                                        \
                                v = glms_mat4_mulv3(m_holder_d, v, 1);  \
                                v = glms_vec3_scale_as(v, K);           \
                                correction = polarisation(v, weight, do_polarisation_correction); \
                                v = glms_vec3_sub(v, ki);               \
                                v = glms_mat4_mulv3(m_holder_s, v, 0);  \
                                                                        \
                                item.indexes_0[0] = rint(v.raw[0] / resolutions[0]); \
                                item.indexes_0[1] = rint(v.raw[1] / resolutions[1]); \
                                item.indexes_0[2] = rint(v.raw[2] / resolutions[2]); \
                                item.intensity = (double)image[i] * correction / flat[i]; \
                                                                        \
                                if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                        darray_append(space->items, item); \
                        }                                               \
                }                                                       \
                                                                        \
                space_update_axes(space, names, n_pixels, resolutions); \
                                                                        \
                hkl_detector_free(detector);                            \
        }

HKL_BINOCULARS_SPACE_TEST_IMPL(double);
HKL_BINOCULARS_SPACE_TEST_IMPL(int32_t);
HKL_BINOCULARS_SPACE_TEST_IMPL(uint16_t);
HKL_BINOCULARS_SPACE_TEST_IMPL(uint32_t);


/* Cube */

/* this method compute the linear coordinates of the first element in
 * the absolute coordinates of the bin */
static inline ptrdiff_t compute_offset0(const darray_axis *axes)
{
	size_t i;
	ptrdiff_t len = 1;
	ptrdiff_t offset = 0;
        size_t n_axes = darray_size(*axes);

	for(i=0; i<n_axes; ++i){
		const HklBinocularsAxis *axis = &darray_item(*axes, n_axes-1-i);
		offset += len * axis->imin;
		len *= axis_size(axis);
	}
	return offset;
}

HklBinocularsCube *empty_cube_from_axes(const darray_axis *axes)
{
        HklBinocularsCube *self = NULL;

        if (0 != darray_size(*axes)){
                HklBinocularsAxis *axis;

                self = g_new0(HklBinocularsCube, 1);
                darray_foreach(axis, *axes){
                        darray_append(self->axes, *axis);
                }
                self->offset0 = compute_offset0(&self->axes);
                self->photons = NULL;
                self->contributions = NULL;
        }

        return self;
}

static inline size_t cube_size(const HklBinocularsCube *self)
{
        size_t n = 1;
        HklBinocularsAxis *axis;

        darray_foreach(axis, self->axes){
                n *= axis_size(axis);
        }

	return n;
}

static inline size_t malloc_cube(HklBinocularsCube *self)
{
        size_t n = cube_size(self);

        self->photons = malloc(n * sizeof(*self->photons));
        self->contributions = malloc(n * sizeof(*self->contributions));

        return n;
}

static inline size_t calloc_cube(HklBinocularsCube *self)
{
        size_t n = cube_size(self);

        self->photons = calloc(n, sizeof(*self->photons));
        self->contributions = calloc(n, sizeof(*self->contributions));

        return n;
}

/* Using this method the Cube has already the right dimensions, we
 * just add the Space data into it. */
static inline void add_non_empty_space(HklBinocularsCube *cube,
                                       const HklBinocularsSpace *space)
{
        size_t i;
        size_t n_axes = darray_size(cube->axes);
        ptrdiff_t lens[n_axes];
        HklBinocularsSpaceItem *item;

        assert(n_axes == darray_size(space->axes));

        /* compute the lens */
        lens[0] = 1;

        for(i=1; i<n_axes; ++i){
                lens[i] = lens[i - 1] * axis_size(&darray_item(cube->axes, n_axes - i));
        }

        darray_foreach(item, space->items){
                ptrdiff_t w = -cube->offset0;

                for(i=0; i<n_axes; ++i){
                        w += lens[i] * item->indexes_0[n_axes - 1 - i];
                        /* fprintf(stdout, " %ld %ld", lens[i], item->indexes_0[n_axes - 1 - i]); */
                }

                /* fprintf(stdout, " w: %ld %ld\n", w, cube_size(cube)); */
                cube->photons[w] += item->intensity;
                cube->contributions[w] += 1;
        }
}

static inline int cube_is_empty(const HklBinocularsCube *self)
{
        return 0 == darray_size(self->axes);
}

HklBinocularsCube *hkl_binoculars_cube_new_empty(void)
{
        HklBinocularsCube *self = g_new(HklBinocularsCube, 1);

        darray_init(self->axes);
        self->offset0 = 0;
        self->photons = NULL;
        self->contributions = NULL;

        return self;
}

void hkl_binoculars_cube_free(HklBinocularsCube *self)
{
        free(self->contributions);
        free(self->photons);
        darray_free(self->axes);
        free(self);
}

unsigned int hkl_binoculars_cube_cmp(const HklBinocularsCube *self, const HklBinocularsCube *other)
{
        int i;
        unsigned int res = 0;

        res |= darray_size(self->axes) != darray_size(other->axes);
        res |= self->offset0 != other->offset0;
        for(i=0; i<darray_size(self->axes); ++i){
                res |= hkl_binoculars_axis_cmp(&darray_item(self->axes, i),
                                               &darray_item(other->axes, i));
        }

        /* for(i=0; i<cube_size(self); ++i){ */
        /*         res |= self->photons[i] != other->photons[i]; */
        /*         res |= self->contributions[i] != other->contributions[i]; */
        /*         if(res){ */
        /*                 fprintf(stdout, "\nphotons: %d %u %u ", i, self->photons[i], other->photons[i]); */
        /*                 fprintf(stdout, "\ncontributions: %d %u %u", i, self->contributions[i], other->contributions[i]); */
        /*                 break; */
        /*         } */
        /* } */

        if(res){
                fprintf(stdout, "cube: self\n");
                hkl_binoculars_cube_fprintf(stdout, self);
                fprintf(stdout, "cube: other\n");
                hkl_binoculars_cube_fprintf(stdout, other);
        }

        return res;
}

void hkl_binoculars_cube_fprintf(FILE *f, const HklBinocularsCube *self)
{
        HklBinocularsAxis *axis;

	fprintf(f, "HklBinocularsCube: %p", self);

        fprintf(f, "\nn_axes: %ld", darray_size(self->axes));
        darray_foreach(axis, self->axes){
                fprintf(f, "\n");
                hkl_binoculars_axis_fprintf(f, axis);
        }
        fprintf(f, "\nphotons: %p", self->photons);
        fprintf(f, "\ncontributions: %p", self->contributions);
}

void hkl_binoculars_cube_dims(const HklBinocularsCube *self, size_t ndims, size_t *dims)
{
        HklBinocularsAxis *axis;

        assert(ndims == darray_size(self->axes));

        darray_foreach(axis, self->axes){
                *(dims++) = axis_size(axis);
        }
}

static inline size_t find_first_non_empty_space_index(size_t n_spaces,
                                                      const HklBinocularsSpace *const *spaces)
{
        size_t i;

        for(i=0; i<n_spaces; ++i)
                if(!space_is_empty(spaces[i]))
                        return i;

        return n_spaces;
}

HklBinocularsCube *hkl_binoculars_cube_new(size_t n_spaces,
                                           const HklBinocularsSpace *const *spaces)
{
        size_t i, i0;
        HklBinocularsCube *self = NULL;

        i0 = find_first_non_empty_space_index(n_spaces, spaces);

        if(i0 < n_spaces){
                self = empty_cube_from_axes(&spaces[i0]->axes);

                if(NULL != self){
                        /* compute the final cube dimensions and the index offset */
                        for(i=i0; i<n_spaces; ++i){
                                const HklBinocularsSpace *space = spaces[i];

                                if(space_is_empty(space))
                                        continue;

                                merge_axes(&self->axes, &space->axes);
                        }
                        self->offset0 = compute_offset0(&self->axes);

                        /* allocated the final cube photons and contributions */
                        calloc_cube(self);

                        /* add all the spaces */
                        for(i=i0; i<n_spaces; ++i){
                                const HklBinocularsSpace *space = spaces[i];

                                if(space_is_empty(space))
                                        continue;

                                add_non_empty_space(self, space);
                        }
                }
        }

	return self;
}

HklBinocularsCube *hkl_binoculars_cube_new_empty_from_cube(const HklBinocularsCube *cube)
{
	HklBinocularsCube *self = empty_cube_from_axes(&cube->axes);
        if(NULL != self)
                calloc_cube(self);
        else{
                self = hkl_binoculars_cube_new_empty();
        }


	return self;
}

HklBinocularsCube *hkl_binoculars_cube_new_from_space(const HklBinocularsSpace *space)
{
	HklBinocularsCube *self = NULL;

        if(space_is_empty(space)){
                self =  hkl_binoculars_cube_new_empty();
        } else {
                self = empty_cube_from_axes(&space->axes);
                if(NULL != self){
                        calloc_cube(self);

                        add_non_empty_space(self, space);
                }
        }

        return self;
}

HklBinocularsCube *hkl_binoculars_cube_new_copy(const HklBinocularsCube *src)
{
        size_t n;
	HklBinocularsCube *self = empty_cube_from_axes(&src->axes);

        if(NULL != self){
                /* allocate the final cube */
                n = malloc_cube(self);

                /* copy the data */
                if(self->photons)
                        memcpy(self->photons, src->photons, n * sizeof(*self->photons));
                if(self->contributions)
                        memcpy(self->contributions, src->contributions, n * sizeof(*self->contributions));
        }

        return self;
}

static inline void cube_add_cube_2(HklBinocularsCube *self,
                                   const HklBinocularsCube *other)
{
        size_t i, j;
        size_t i_offset, j_offset;

        size_t stride_i = 1;
        size_t stride_j = stride_i * axis_size(&darray_item(self->axes, 1));

        /* fill the values of other */
        size_t stride_i_other = 1;
        size_t stride_j_other = stride_i_other * axis_size(&darray_item(other->axes, 1));

        i_offset = darray_item(other->axes, 1).imin - darray_item(self->axes, 1).imin;
        j_offset = darray_item(other->axes, 0).imin - darray_item(self->axes, 0).imin;

        for(j=0; j<axis_size(&darray_item(other->axes, 0)); ++j){
                for(i=0; i<axis_size(&darray_item(other->axes, 1)); ++i){
                        size_t w = (i + i_offset) * stride_i + (j + j_offset) * stride_j;
                        size_t w1 = i * stride_i_other + j * stride_j_other;

                        self->photons[w] += other->photons[w1];
                        self->contributions[w] += other->contributions[w1];
                }
        }
}

static inline void cube_add_cube_3(HklBinocularsCube *self,
                                   const HklBinocularsCube *other)
{
        size_t i, j, k;
        size_t i_offset, j_offset, k_offset;

        size_t stride_i = 1;
        size_t stride_j = stride_i * axis_size(&darray_item(self->axes, 2));
        size_t stride_k = stride_j * axis_size(&darray_item(self->axes, 1));

        /* fill the values of other */
        size_t stride_i_other = 1;
        size_t stride_j_other = stride_i_other * axis_size(&darray_item(other->axes, 2));
        size_t stride_k_other = stride_j_other * axis_size(&darray_item(other->axes, 1));

        i_offset = darray_item(other->axes, 2).imin - darray_item(self->axes, 2).imin;
        j_offset = darray_item(other->axes, 1).imin - darray_item(self->axes, 1).imin;
        k_offset = darray_item(other->axes, 0).imin - darray_item(self->axes, 0).imin;

        for(k=0; k<axis_size(&darray_item(other->axes, 0)); ++k){
                for(j=0; j<axis_size(&darray_item(other->axes, 1)); ++j){
                        for(i=0; i<axis_size(&darray_item(other->axes, 2)); ++i){
                                size_t w = (i + i_offset) * stride_i + (j + j_offset) * stride_j + (k + k_offset) * stride_k;
                                size_t w1 = i * stride_i_other + j * stride_j_other + k * stride_k_other;

                                self->photons[w] += other->photons[w1];
                                self->contributions[w] += other->contributions[w1];
                        }
                }
        }
}

static inline void cube_add_cube(HklBinocularsCube *self,
                                 const HklBinocularsCube *other)
{
        assert(darray_size(self->axes) == darray_size(other->axes));

        switch(darray_size(self->axes)){
        case 2: cube_add_cube_2(self, other);
                break;
        case 3: cube_add_cube_3(self, other);
                break;
        default: assert(0);
        }
}

static inline void compute_strides(const darray_axis *axes, size_t strides[], size_t n_strides)
{
        size_t i;

        strides[0] = 1;
        for(i=1; i<n_strides; ++i)
                strides[i] = strides[i-1] * axis_size(&darray_item(*axes, n_strides - i + 1));
}

HklBinocularsCube *hkl_binoculars_cube_new_merge(const HklBinocularsCube *cube1,
                                                 const HklBinocularsCube *cube2)
{
        HklBinocularsCube *self = empty_cube_from_axes(&cube1->axes);
        if(NULL != self){
                merge_axes(&self->axes, &cube2->axes);
                calloc_cube(self);

                cube_add_cube(self, cube1);
                cube_add_cube(self, cube2);
        }
        return self;
}

static inline void switch_content(HklBinocularsCube *self,
                                  HklBinocularsCube *other)
{
        void *ptr;
        darray_axis tmp;
        ptrdiff_t offset0;

        tmp = self->axes;
        self->axes = other->axes;
        other->axes = tmp;

        offset0 = self->offset0;
        self->offset0 = other->offset0;
        other->offset0 = offset0;

        ptr = self->photons;
        self->photons = other->photons;
        other->photons = ptr;

        ptr = self->contributions;
        self->contributions = other->contributions;
        other->contributions = ptr;
}

void hkl_binoculars_cube_add_cube(HklBinocularsCube *self,
                                  const HklBinocularsCube *other)
{
#ifdef DEBUG
        fprintf(stdout, "\nENTERING hkl_binoculars_cube_add_cube:\n");
        hkl_binoculars_cube_fprintf(stdout, self);
        hkl_binoculars_cube_fprintf(stdout, other);
#endif
        /* check the compatibility of the cube(self) and the cube(other). */
        if (1 != cube_is_empty(other)){
                if (does_not_include(&self->axes, &other->axes)){
#ifdef DEBUG
                        fprintf(stdout, "\nthe first Cube does not contain the second one, so create a new cube.");
#endif
                        if(0 != darray_size(self->axes)){ /* self cube is not empty */
                                HklBinocularsCube *cube = empty_cube_from_axes(&self->axes);
                                if(NULL != cube){
                                        merge_axes(&cube->axes, &other->axes); /* circonscript */
                                        cube->offset0 = compute_offset0(&cube->axes);
                                        calloc_cube(cube);
                                        cube_add_cube(cube, self);
                                        switch_content(self, cube);
                                        hkl_binoculars_cube_free(cube);
                                }
                        } else { /* self cube is empty */
                                HklBinocularsCube *cube =  empty_cube_from_axes(&other->axes);
                                if(NULL != cube){
                                        cube->offset0 = compute_offset0(&cube->axes);
                                        calloc_cube(cube);
                                        switch_content(self, cube);
                                        hkl_binoculars_cube_free(cube);
                                }
                        }
                }
                cube_add_cube(self, other);
        }

#ifdef DEBUG
        fprintf(stdout, "\n");
        hkl_binoculars_cube_fprintf(stdout, self);
        fprintf(stdout, "\nLEAVING hkl_binoculars_cube_add_cube:\n");
#endif
}

void hkl_binoculars_cube_add_space(HklBinocularsCube *self,
                                   const HklBinocularsSpace *space)
{
#ifdef DEBUG
        fprintf(stdout, "\nENTERING hkl_binoculars_cube_add_space:\n");
        hkl_binoculars_cube_fprintf(stdout, self);
        hkl_binoculars_space_fprintf(stdout, space);
#endif
        /* check the compatibility of the cube and the space. */
        if (1 != space_is_empty(space)){
                if (does_not_include(&self->axes, &space->axes)){
#ifdef DEBUG
                        fprintf(stdout, "\nthe Cube does not contain the space, so create a new cube.");
#endif
                        if(0 != darray_size(self->axes)){ /* self cube is not empty */
                                HklBinocularsCube *cube = empty_cube_from_axes(&self->axes);
                                if(NULL != cube){
                                        merge_axes(&cube->axes, &space->axes); /* circonscript */
                                        cube->offset0 = compute_offset0(&cube->axes);
                                        calloc_cube(cube);
                                        cube_add_cube(cube, self);
                                        switch_content(self, cube);
                                        hkl_binoculars_cube_free(cube);
                                }
                        } else { /* self cube is empty */
                                HklBinocularsCube *cube =  empty_cube_from_axes(&space->axes);
                                if(NULL != cube){
                                        cube->offset0 = compute_offset0(&cube->axes);
                                        calloc_cube(cube);
                                        switch_content(self, cube);
                                        hkl_binoculars_cube_free(cube);
                                }
                        }
                }
                add_non_empty_space(self, space);
        }

#ifdef DEBUG
        fprintf(stdout, "\n");
        hkl_binoculars_cube_fprintf(stdout, self);
        fprintf(stdout, "\nLEAVING hkl_binoculars_cube_add_space:\n");
#endif
}
