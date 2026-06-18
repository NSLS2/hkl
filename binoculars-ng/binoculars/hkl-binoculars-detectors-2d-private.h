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
#ifndef __HKL_BINOCULARS_DETECTORS_2D_PRIVATE_H__
#define __HKL_BINOCULARS_DETECTORS_2D_PRIVATE_H__

#include <stdbool.h>
#include <stdlib.h>

#include <cglm/struct.h>
#include <cglm/version.h>

#include "datatype99.h"

#include "hkl/ccan/array_size/array_size.h"

#include "hkl-binoculars.h"

/**********/
/* macros */
/**********/

#define wrong_detector(n) fprintf(stderr, "You requested a non existing detector %d the maximum number available is %d", n, HKL_BINOCULARS_DETECTOR_NUM_DETECTORS)

#define shape_size(shape) (shape).width * (shape).height
#define flat_index(shape, i, j) i + j * (shape).width

#define get_row(arr, shape, i) &arr[(i) * (shape).width]
#define get_col(arr, i) &arr[i]

#define replicate_row(row, shape, n)					\
	do{								\
		for(int i_=1; i_<(n); ++i_){				\
			memcpy(&(row)[i_ * (shape).width], (row), (shape).width * sizeof(*(row))); \
		}							\
	} while(0)

#define fill_row(row, shape, val)			\
	do{						\
		for(int i_=0; i_<(shape).width; ++i_){	\
			(row)[i_] = (val);		\
		}					\
	} while(0)

#define replicate_column(col, shape, n)					\
	do{								\
                for(int i_=0; i_<shape_size(shape); i_=i_+(shape).width){ \
                        for(int j_=1; j_<(n); ++j_){                    \
                                (col[i_+j_]) = col[i_];                 \
                        }                                               \
                }                                                       \
        } while(0)

#define fill_column(col, shape, val)					\
	do {								\
                for(int i_=0; i_<shape_size(shape); i_=i_+(shape).width){ \
                        (col)[i_] = (val);                              \
                }                                                       \
        } while(0)

#define x_coordinates(arr, shape) &arr[0 * shape_size(shape)]
#define y_coordinates(arr, shape) &arr[1 * shape_size(shape)]
#define z_coordinates(arr, shape) &arr[2 * shape_size(shape)]

/*************/
/* detectors */
/************/

struct shape_t {
        int width;
        int height;
};

#define SHAPE(width_, height_) (struct shape_t) \
        {.width=width_, .height=height_}

struct square_t {
        double pixel_size;
};

#define SQUARE(pixel_size_) (struct square_t)	\
        {.pixel_size=pixel_size_}

struct imxpad_t {
        struct square_t square;
        int chip_w;
        int chip_h;
};

#define IMXPAD(pixel_size_, chip_w_, chip_h_) (struct imxpad_t)		\
        {.square=SQUARE(pixel_size_), .chip_w=chip_w_, .chip_h=chip_h_}

#define CIRPAD_N_IMXPAD_S70 20

#define CIRPAD_PARAMETERS {                                             \
		{{{0.64907 - 5.0e-3, 0, 0}}, {{glm_rad(90), glm_rad( 0.0000 + 0.012000), 0}}}, \
		{{{0.64907 - 4.0e-3, 0, 0}}, {{glm_rad(90), glm_rad(-6.7400 + 0.010000), 0}}}, \
		{{{0.64907 - 5.0e-3, 0, 0}}, {{glm_rad(90), glm_rad(-13.480 - 0.000000), 0}}}, \
		{{{0.64907 - 5.1e-3, 0, 0}}, {{glm_rad(90), glm_rad(-20.220 - 0.000000), 0}}}, \
		{{{0.64907 - 5.6e-3, 0, 0}}, {{glm_rad(90), glm_rad(-26.960 - 0.000000), 0}}}, \
		{{{0.64907 - 5.3e-3, 0, 0}}, {{glm_rad(90), glm_rad(-33.700 - 0.050000), 0}}}, \
		{{{0.64907 - 5.5e-3, 0, 0}}, {{glm_rad(90), glm_rad(-40.440 - 0.080000), 0}}}, \
		{{{0.64907 - 5.8e-3, 0, 0}}, {{glm_rad(90), glm_rad(-47.180 - 0.000000), 0}}}, \
		{{{0.64907 - 5.6e-3, 0, 0}}, {{glm_rad(90), glm_rad(-53.920 - 0.070000), 0}}}, \
		{{{0.64907 - 5.8e-3, 0, 0}}, {{glm_rad(90), glm_rad(-60.660 - 0.000000), 0}}}, \
		{{{0.64907 - 6.2e-3, 0, 0}}, {{glm_rad(90), glm_rad(-67.400 - 0.000000), 0}}}, \
		{{{0.64907 - 5.5e-3, 0, 0}}, {{glm_rad(90), glm_rad(-74.140 - 0.000000), 0}}}, \
		{{{0.64907 - 5.5e-3, 0, 0}}, {{glm_rad(90), glm_rad(-80.880 - 0.000000), 0}}}, \
		{{{0.64907 - 5.7e-3, 0, 0}}, {{glm_rad(90), glm_rad(-87.620 - 0.060000), 0}}}, \
		{{{0.64907 - 5.0e-3, 0, 0}}, {{glm_rad(90), glm_rad(-94.360 - 0.000000), 0}}}, \
		{{{0.64907 - 5.0e-3, 0, 0}}, {{glm_rad(90), glm_rad(-101.10 + 0.050000), 0}}}, \
		{{{0.64907 - 5.0e-3, 0, 0}}, {{glm_rad(90), glm_rad(-107.84 - 0.000000), 0}}}, \
		{{{0.64907 - 4.0e-3, 0, 0}}, {{glm_rad(90), glm_rad(-114.58 - 0.000000), 0}}}, \
		{{{0.64907 - 4.0e-3, 0, 0}}, {{glm_rad(90), glm_rad(-121.32 + 0.010000), 0}}}, \
		{{{0.64907 - 4.0e-3, 0, 0}}, {{glm_rad(90), glm_rad(-128.06 + 0.010000), 0}}}, \
			}

struct cirpad_transformation_t {
	CGLM_ALIGN_MAT vec3s translation;
	CGLM_ALIGN_MAT vec3s eulers;
};

struct cirpad_t {
        struct imxpad_t imxpad_s70;
        struct shape_t imxpad_s70_shape;
        size_t n_imxpad_s70;
        struct cirpad_transformation_t transformations[CIRPAD_N_IMXPAD_S70];
};


#define CIRPAD(pixel_size_, chip_w_, chip_h_) (struct cirpad_t)	\
        { .imxpad_s70=IMXPAD(pixel_size_, chip_w_, chip_h_)	\
                        , .imxpad_s70_shape=SHAPE(560, 120)	\
                        , .n_imxpad_s70=CIRPAD_N_IMXPAD_S70	\
                        , .transformations=CIRPAD_PARAMETERS	\
                        }

struct tilling_t {
        struct square_t square;
        int module_width;
        int module_height;
        int gap_width;
        int gap_height;
        bool gap_in_data;
        bool gap_masked;
        int border_masked;
};

#define TILLING(module_width_, module_height_, gap_width_, gap_height_, pixel_size_, gap_in_data_, gap_masked_, border_masked_) \
        (struct tilling_t){                                             \
                .square=SQUARE(pixel_size_),                            \
                        .module_width=module_width_,                    \
                        .module_height=module_height_,                  \
                        .gap_width=gap_width_,                          \
                        .gap_height=gap_height_,                        \
                        .gap_in_data=gap_in_data_,                      \
                        .gap_masked=gap_masked_,                        \
                        .border_masked=border_masked_,                  \
                        }

struct pilatus_cdte_t {
	struct tilling_t tilling;
};

#define PILATUS_CDTE							\
	(struct pilatus_cdte_t){					\
		.tilling=TILLING(487, 195, 7, 17, 172e-6, true, true, 0) \
			}

datatype(
        DetectorType,
        (ImXpadS70, struct imxpad_t),
        (ImXpadS140, struct imxpad_t),
        (XpadFlatCorrected, struct square_t),
        (Eiger1M, struct tilling_t),
        (Ufxc, struct square_t),
        (Merlin, struct square_t),
        (MerlinMedipix3RXQuad, struct tilling_t),
        (MerlinMedipix3RXQuad512, struct tilling_t),
        (Cirpad, struct cirpad_t),
	(RigakuXspa1M, struct square_t),
	(PilatusCdTe2M, struct pilatus_cdte_t)
        );

struct detector_t {
        const char *name;
        const struct shape_t shape;
        DetectorType type;
};

#define DETECTOR(name_, shape_, type_) (struct detector_t)      \
        {.name=#name_, .shape=shape_, .type=name_(type_)}

static inline struct detector_t get_detector(HklBinocularsDetectorEnum n)
{
        struct detector_t detectors[] = {
                DETECTOR(ImXpadS140,
                         SHAPE(560, 240), IMXPAD(130e-6, 80, 120)),
                DETECTOR(XpadFlatCorrected,
                         SHAPE(576, 1154), SQUARE(130e-6)),
                DETECTOR(ImXpadS70,
                         SHAPE(560, 120), IMXPAD(130e-6, 80, 120)),
                DETECTOR(Eiger1M,
                         SHAPE(1030, 1065), TILLING(1030, 514, 10, 37, 75e-6, true, true, 0)),
                DETECTOR(Ufxc,
                         SHAPE(257, 256), SQUARE(75e-6)),
                DETECTOR(Merlin,
                         SHAPE(256, 256), SQUARE(55e-6)),
                DETECTOR(MerlinMedipix3RXQuad,
                         SHAPE(515, 515), TILLING(256, 256, 3, 3, 55e-6, true, true, 0)),
                DETECTOR(MerlinMedipix3RXQuad512,
                         SHAPE(512, 512), TILLING(256, 256, 3, 3, 55e-6, false, false, 2)),
                DETECTOR(Cirpad,
                         SHAPE(560, 2400), CIRPAD(130e-6, 80, 120)),
		DETECTOR(RigakuXspa1M,
			 SHAPE(1034, 1104), SQUARE(76e-6)),
                DETECTOR(PilatusCdTe2M,
                         SHAPE(1475, 1679), PILATUS_CDTE),
        };

        if (n > ARRAY_SIZE(detectors))
                n = 0; /* use the first detector as default */

        return detectors[n];
}

#endif
