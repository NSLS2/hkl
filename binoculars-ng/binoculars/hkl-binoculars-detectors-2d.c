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
#include <stdbool.h>
#include <stdlib.h>

#include <cglm/struct.h>
#include <cglm/version.h>

#include "datatype99.h"

#include "hkl/ccan/array_size/array_size.h"

#include "hkl-binoculars.h"
#include "hkl-binoculars-detectors-2d-private.h"
#include "hkl-binoculars-io-private.h"
#include "hkl-quaternion-private.h"
#include "hkl-vector-private.h"


/***********************/
/* specific operations */
/***********************/

/* coordinates */

static double *coordinates_new(const struct shape_t *shape)
{
        return g_new0(double, 3 * shape_size(*shape));
}

static double tilling_coordinates_pattern(int i,
					  double pixel_size,
					  int module_size, int gap_size)
{
        div_t q = div(i, module_size);

        return (i + q.quot * gap_size + 0.5) * pixel_size;
}


static double imxpad_coordinates_pattern(int i, int chip, double s)
{
        div_t q = div(i, chip);

        if (i == 0){
                return s / 2;
        }
        if (i == 1){
                return s * 3.0 / 2.0;
        }
        if (q.rem == 0){
                return s * (i + 3 * q.quot - 0.25);
        }
        if (q.rem <= (chip - 2)) {
                return s * (i + 3 * q.quot + 0.5);
        }
        if (q.rem <= (chip - 1)) {
                return s * (i + 3 * q.quot + 2.5);
        }
        return NAN;
}

static double *coordinates_rectangle(const struct shape_t *shape,
				     double p_w, double p_h)
{
        int i;
        double *arr = coordinates_new(shape);
        double *y, *z;

        /* y */
        y = y_coordinates(arr, *shape);
        for(i=0; i<shape->width; ++i)
                y[i] = - (0.5 + i) * p_w;
        replicate_row(y, *shape, shape->height);

        /* z */
        z = z_coordinates(arr, *shape);
        for(i=0; i<shape->height; ++i){
                double *row = get_row(z, *shape, i);
                fill_row(row, *shape, (0.5 + i) * p_h);
        }

        return arr;

}

static double* coordinates_get_tilling(const struct shape_t *shape,
				       const struct tilling_t *tilling)
{
        int i;
        double *arr;

        if (tilling->gap_in_data){
                arr = coordinates_rectangle(shape,
                                            tilling->square.pixel_size,
                                            tilling->square.pixel_size);
        }else{
                double *z, *y;

                arr = coordinates_new(shape);
                /* y */
                y = y_coordinates(arr, *shape);
                for(i=0; i<shape->width; ++i){
                        y[i] = - tilling_coordinates_pattern(i,
                                                             tilling->square.pixel_size,
                                                             tilling->module_width,
                                                             tilling->gap_width);
                }
                replicate_row(y, *shape, shape->height);

                /* z */
                z = z_coordinates(arr, *shape);
                for(i=0; i<shape->height; ++i){
                        double *row = get_row(z, *shape, i);
                        fill_row(row, *shape,
                                 tilling_coordinates_pattern(i,
                                                             tilling->square.pixel_size,
                                                             tilling->module_height,
                                                             tilling->gap_height));
                }
        }
        return arr;
}

static void coordinates_set_imxpad_xyz(double *y, double *z,
				       const struct shape_t *shape,
				       const struct imxpad_t *imxpad)
{
        int i;
        double *row;

        /* x = 0 (nothing to do) */

        /* y */
        row = y;
        for(i=0; i<shape->width; ++i){
                row[i] = - imxpad_coordinates_pattern(i,
                                                      imxpad->chip_w,
                                                      imxpad->square.pixel_size);
        }
        replicate_row(row, *shape, shape->height);

        /* z */
        for(i=0; i<shape->height; ++i){
                row = get_row(z, *shape, i);
                fill_row(row, *shape,
                         imxpad_coordinates_pattern(i,
                                                    imxpad->chip_h,
                                                    imxpad->square.pixel_size));
        }
}

static double *coordinates_get_imxpad(const struct shape_t *shape,
				      const struct imxpad_t *imxpad)
{
        double *arr = coordinates_new(shape);

        double *y = y_coordinates(arr, *shape);
        double *z = z_coordinates(arr, *shape);

        coordinates_set_imxpad_xyz(y, z, shape, imxpad);

        return arr;
}

static double *coordinates_get_cirpad(const struct shape_t *shape,
				      const struct cirpad_t *cirpad)
{
        int i;
        double *arr = coordinates_new(shape);
        double *x = x_coordinates(arr, *shape);
        double *y = y_coordinates(arr, *shape);
        double *z = z_coordinates(arr, *shape);

        for(i=0; i<cirpad->n_imxpad_s70; ++i){
		size_t j;
		CGLM_ALIGN_MAT mat4s t_m = GLMS_MAT4_IDENTITY_INIT;
		CGLM_ALIGN_MAT mat4s r_m = GLMS_MAT4_IDENTITY_INIT;
                double *x_imxpad = x + i * shape_size(cirpad->imxpad_s70_shape);
                double *y_imxpad = y + i * shape_size(cirpad->imxpad_s70_shape);
                double *z_imxpad = z + i * shape_size(cirpad->imxpad_s70_shape);

                /* set the default module coordinates */

                coordinates_set_imxpad_xyz(y_imxpad, z_imxpad,
                                           &cirpad->imxpad_s70_shape, &cirpad->imxpad_s70);

                /* apply the transformations for each module */

		t_m = glms_translate_make(cirpad->transformations[i].translation);
		/* glms_mat4_mul(t_m, glms_translate_make(v_t)); */
		r_m = glms_euler_zyx(cirpad->transformations[i].eulers);

		for(j=0; j<shape_size(cirpad->imxpad_s70_shape); ++j){
			CGLM_ALIGN_MAT vec3s v = {{x_imxpad[j], y_imxpad[j], z_imxpad[j]}};
			v = glms_mat4_mulv3(t_m, v, 1);
			v = glms_mat4_mulv3(r_m, v, 0);

			x_imxpad[j] = v.raw[0], y_imxpad[j] = v.raw[1], z_imxpad[j] = v.raw[2];
		}
        }

        return arr;
}

static void flip_z(const struct shape_t *shape, double *arr)
{
        int i;
        double *z = z_coordinates(arr, *shape);

        for(i=0; i<shape_size(*shape); ++i)
                z[i] *= -1;
}

static double *coordinates_get_square(const struct shape_t *shape,
				      const struct square_t *square)
{
        return coordinates_rectangle(shape,
                                     square->pixel_size,
                                     square->pixel_size);
}

/* flat */

static double *default_flat(const struct shape_t *shape)
{
	size_t i;
	double *arr;

        arr = g_new(double, shape_size(*shape));
	for(i=0; i<shape_size(*shape); ++i) arr[i] = 1;

	return arr;
}

/* masks */

static uint8_t *no_mask(const struct shape_t *shape)
{
        return g_new0(uint8_t, shape_size(*shape));
}

static void mask_add_vertical_strip(uint8_t *arr, const struct shape_t *shape, size_t origin, size_t thickness)
{
	if(origin < shape->width && origin + thickness <= shape->width){
		uint8_t *col = get_col(arr, origin);
		fill_column(col, *shape, 1);
		replicate_column(col, *shape, thickness);
	}
}

static void mask_add_horizontal_strip(uint8_t *arr, const struct shape_t *shape, size_t origin, size_t thickness)
{
	if(origin < shape->height && origin + thickness <= shape->height){
		uint8_t *row = get_row(arr, *shape, origin);
		fill_row(row, *shape, 1);
		replicate_row(row, *shape, thickness);
	}
}

static void mask_add_ring(uint8_t *arr, const struct shape_t *shape, size_t thickness)
{
	mask_add_vertical_strip(arr, shape, 0, thickness);
	mask_add_vertical_strip(arr, shape, shape->width - thickness, thickness);

	mask_add_horizontal_strip(arr, shape, 0, thickness);
	mask_add_horizontal_strip(arr, shape, shape->height - thickness, thickness);
}

static uint8_t *mask_get_imxpad(const struct shape_t *shape,
				const struct imxpad_t *imxpad)
{
        uint8_t *arr = no_mask(shape);

        div_t q = div(shape->width, imxpad->chip_w);
        int n_chips = q.quot;

        for(int chip=0; chip<n_chips; ++chip){
                if (chip != 0)
			mask_add_vertical_strip(arr, shape, chip * imxpad->chip_w, 1);

                if (chip != (n_chips - 1))
			mask_add_vertical_strip(arr, shape, (chip + 1) * imxpad->chip_w - 1, 1);
        }

        q = div(shape->height, imxpad->chip_h);
        int n_modules = q.quot;

        for(int module=0; module<n_modules; ++module){
                if (module != 0)
			mask_add_horizontal_strip(arr, shape, module * imxpad->chip_h, 1);

                if (module != (n_modules - 1))
			mask_add_horizontal_strip(arr, shape, (module + 1) * imxpad->chip_h - 1, 1);
        }

        return arr;
}

static uint8_t *mask_get_xpad_flat_corrected(const struct shape_t *shape)
{
        uint8_t *arr = no_mask(shape);

        /* now mask all the strange row */
        for(int i=118; i<=1006; i=i+148)
		mask_add_horizontal_strip(arr, shape, i, 30);

        return arr;
}

static uint8_t *mask_get_tilling(const struct shape_t *shape,
				 const struct tilling_t *tilling)
{
        int i;
        uint8_t *arr = no_mask(shape);


        /* take care of the gap */
        if(tilling->gap_in_data && tilling->gap_masked){
                /* columns */
                for(i=tilling->module_width;
                    i<shape->width;
                    i=i+tilling->module_width + tilling->gap_width){
			mask_add_vertical_strip(arr, shape, i, tilling->gap_width);
                }

                /* rows */
                for(i=tilling->module_height;
                    i<shape->height;
                    i=i+tilling->module_height + tilling->gap_height){
			mask_add_horizontal_strip(arr, shape, i, tilling->gap_height);
                }
        }

        /* border */
        /* TODO deal with the gap */
        if (tilling->border_masked > 0){
                /* columns */
                for(i=0;
                    i<shape->width;
                    i = i + tilling->module_width){
			mask_add_vertical_strip(arr, shape, i,
						tilling->border_masked);
			mask_add_vertical_strip(arr, shape, i + tilling->module_width - tilling->border_masked,
						tilling->border_masked);
                }

                /* rows */
                for(i=0;
                    i<shape->height;
                    i = i + tilling->module_height){
			mask_add_horizontal_strip(arr, shape, i,
                                                  tilling->border_masked);
			mask_add_horizontal_strip(arr, shape, i + tilling->module_height - tilling->border_masked,
						  tilling->border_masked);
                }
        }

        return arr;
}


static uint8_t *mask_get_rigaku_xspa_1m(const struct shape_t *shape)
{
        uint8_t *arr = no_mask(shape);

	mask_add_ring(arr, shape, 5);

	mask_add_horizontal_strip(arr, shape, 512 + 5, 70);

        return arr;
}

/***************/
/* Calibration */
/***************/

static void translate_coordinates_xyz(double *x, double *y, double *z,
				      const struct shape_t *shape,
				      double dx, double dy, double dz)
{
        size_t i;

        for(i=0; i<shape_size(*shape); ++i){
                x[i] += dx;
                y[i] += dy;
                z[i] += dz;
        }
}

static void rotate_coordinates_xyz(double *x, double *y, double *z,
				   const struct shape_t *shape,
				   double angle,
				   double axis_x, double axis_y, double axis_z)
{
        HklVector axis = {{axis_x, axis_y, axis_z}};
        HklQuaternion q;

        hkl_quaternion_init_from_angle_and_axe(&q, angle, &axis);

        for(int i=0; i<shape_size(*shape); ++i){
                HklVector v= {{x[i], y[i], z[i]}};

                hkl_vector_rotated_quaternion(&v, &q);
                x[i] = v.data[0];
                y[i] = v.data[1];
                z[i] = v.data[2];
        }
}

static void normalize_coordinates_xyz(double *x, double *y, double *z,
				      const struct shape_t shape)
{
        for(int i=0; i<shape_size(shape); ++i)
        {
                double n = sqrt(x[i] * x[i] + y[i] * y[i] + z[i] * z[i]);
                if (n != 0.0){
                        x[i] = x[i] / n;
                        y[i] = y[i] / n;
                        z[i] = z[i] / n;
                }
        }
}

void hkl_binoculars_detector_2d_sixs_calibration(HklBinocularsDetectorEnum n,
                                                 double *arr,
                                                 int width, int height,
                                                 int ix0, int iy0, double sdd,
                                                 double detrot, int normalize_flag)
{
        const struct shape_t shape = SHAPE(width, height);
        double *x = x_coordinates(arr, shape);
        double *y = y_coordinates(arr, shape);
        double *z = z_coordinates(arr, shape);

        double dx = sdd;
        double dy = -y[flat_index(shape, ix0, iy0)];
        double dz = -z[flat_index(shape, ix0, iy0)];

        translate_coordinates_xyz(x, y, z, &shape, dx, dy, dz);
        rotate_coordinates_xyz(x, y, z, &shape, detrot, 1, 0, 0);
        if(normalize_flag)
                normalize_coordinates_xyz(x, y, z, shape);
}

/*****************************/
/* public API implementation */
/*****************************/

int hkl_binoculars_detector_2d_number_of_detectors(void)
{
        return HKL_BINOCULARS_DETECTOR_NUM_DETECTORS;
};

const char *hkl_binoculars_detector_2d_name_get(HklBinocularsDetectorEnum n)
{
        const struct detector_t detector = get_detector(n);

        return detector.name;
};

void hkl_binoculars_detector_2d_shape_get(HklBinocularsDetectorEnum n,
                                          int *width, int *height)
{
        const struct detector_t detector = get_detector(n);

        *width = detector.shape.width;
        *height = detector.shape.height;
}

double *hkl_binoculars_detector_2d_coordinates_get(HklBinocularsDetectorEnum n)
{
        const struct detector_t detector = get_detector(n);
        double *arr = NULL;

        match(detector.type){
                of(ImXpadS70, imxpad){
                        arr = coordinates_get_imxpad(&detector.shape,
                                                     imxpad);
                }
                of(ImXpadS140, imxpad){
                        arr = coordinates_get_imxpad(&detector.shape,
                                                     imxpad);
                }
                of(XpadFlatCorrected, square){
                        arr = coordinates_get_square(&detector.shape,
                                                     square);
                }
                of(Eiger1M, tilling){
                        arr = coordinates_get_square(&detector.shape,
                                                     &tilling->square);
                        flip_z(&detector.shape, arr);
                }
                of(Ufxc, square){
                        arr = coordinates_get_square(&detector.shape,
                                                     square);
                }
                of(Merlin, square){
                        arr = coordinates_get_square(&detector.shape,
                                                     square);
                }
                of(MerlinMedipix3RXQuad, tilling){
                        arr = coordinates_get_square(&detector.shape,
                                                     &tilling->square);
                }
                of(MerlinMedipix3RXQuad512, tilling){
                        arr = coordinates_get_tilling(&detector.shape,
                                                      tilling);
                }
                of(Cirpad, cirpad){
                        arr = coordinates_get_cirpad(&detector.shape,
                                                     cirpad);
                }
		of(RigakuXspa1M, square){
			arr = coordinates_get_square(&detector.shape,
						     square);
                        flip_z(&detector.shape, arr);
		}
                of(PilatusCdTe2M, tilling){
                        arr = coordinates_get_square(&detector.shape,
                                                     &tilling->square);
                        flip_z(&detector.shape, arr);
                }
        }
        return arr;
}

void hkl_binoculars_detector_2d_coordinates_save(HklBinocularsDetectorEnum n,
                                                 const char *fname)
{
        double *arr = NULL;
        const struct detector_t detector = get_detector(n);

        darray_int shape = darray_new();

        darray_append(shape, 3);
        darray_append(shape, detector.shape.height);
        darray_append(shape, detector.shape.width);

        arr = hkl_binoculars_detector_2d_coordinates_get(n);

        npy_save(fname, arr, HklBinocularsNpyDouble(), &shape);

        free(arr);
        darray_free(shape);
}


double *hkl_binoculars_detector_2d_flat_get(HklBinocularsDetectorEnum n)
{
        const struct detector_t detector = get_detector(n);
        double *arr;

	arr = default_flat(&detector.shape);

        return arr;
}

double *hkl_binoculars_detector_2d_flat_load(HklBinocularsDetectorEnum n,
					     const char *fname)
{
        double *arr = NULL;
        const struct detector_t detector = get_detector(n);
        darray_int shape = darray_new();

        darray_append(shape, detector.shape.height);
        darray_append(shape, detector.shape.width);

        arr = npy_load(fname, HklBinocularsNpyDouble(), &shape);

        darray_free(shape);

        return arr;
};

uint32_t *hkl_binoculars_detector_2d_fake_image_uint32(HklBinocularsDetectorEnum n,
                                                       size_t *n_pixels)
{
        size_t i;
        uint32_t *arr;
        const struct detector_t detector = get_detector(n);

        *n_pixels = detector.shape.width * detector.shape.height;
        arr = g_new(uint32_t, *n_pixels);
        for(i=0; i<*n_pixels; ++i){
                arr[i] = rand();
        }

        return arr;
}

uint8_t *hkl_binoculars_detector_2d_mask_get(HklBinocularsDetectorEnum n)
{
        const struct detector_t detector = get_detector(n);
        uint8_t *arr;

        match(detector.type){
                of(ImXpadS70, imxpad){
                        arr = mask_get_imxpad(&detector.shape,
                                              imxpad);
                }
                of(ImXpadS140, imxpad){
                        arr = mask_get_imxpad(&detector.shape,
					      imxpad);
                }
                of(XpadFlatCorrected){
                        arr = mask_get_xpad_flat_corrected(&detector.shape);
                }
                of(Eiger1M, tilling){
                        arr = mask_get_tilling(&detector.shape,
                                               tilling);
                }
                of(Ufxc){
                        arr = no_mask(&detector.shape);
                }
                of(Merlin){
                        arr = no_mask(&detector.shape);
                }
                of(MerlinMedipix3RXQuad, tilling){
                        arr = mask_get_tilling(&detector.shape,
                                               tilling);
                }
                of(MerlinMedipix3RXQuad512, tilling){
                        arr = mask_get_tilling(&detector.shape,
                                               tilling);
                }
                of(Cirpad, _){
                        arr = no_mask(&detector.shape);
                }
		of(RigakuXspa1M, _){
			arr = mask_get_rigaku_xspa_1m(&detector.shape);
		}
                of(PilatusCdTe2M, tilling){
                        arr = mask_get_tilling(&detector.shape,
                                               tilling);
                }
                otherwise {
                        arr = no_mask(&detector.shape);
                }
        }

        return arr;
}

uint8_t *hkl_binoculars_detector_2d_mask_load(HklBinocularsDetectorEnum n,
                                              const char *fname)
{
        uint8_t *arr = NULL;
        const struct detector_t detector = get_detector(n);
        darray_int shape = darray_new();

        darray_append(shape, detector.shape.height);
        darray_append(shape, detector.shape.width);

        arr = npy_load(fname, HklBinocularsNpyBool(), &shape);

        darray_free(shape);

        return arr;
};

uint8_t *hkl_binoculars_detector_2d_mask_or(HklBinocularsDetectorEnum n,
                                            uint8_t *arr_l, uint8_t *arr_r)
{
        size_t i;
        const struct detector_t detector = get_detector(n);
        uint8_t *arr = no_mask(&detector.shape);

        if (arr != NULL)
                for(i=0; i<shape_size(detector.shape); ++i)
                        arr[i] = arr_l[i] | arr_r[i];

        return arr;
}

void hkl_binoculars_detector_2d_mask_save(HklBinocularsDetectorEnum n,
                                          const char *fname)
{
        uint8_t *arr = NULL;
        const struct detector_t detector = get_detector(n);
        darray_int shape = darray_new();

        darray_append(shape, detector.shape.height);
        darray_append(shape, detector.shape.width);

        arr = hkl_binoculars_detector_2d_mask_get(n);
        npy_save(fname, arr, HklBinocularsNpyBool(), &shape);

        darray_free(shape);
        free(arr);
}
