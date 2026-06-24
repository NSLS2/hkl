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
#ifndef __HKL_BINOCULARS_H__
#define __HKL_BINOCULARS_H__

#include <stdint.h>
#include <stdbool.h>

#include "hkl.h"

G_BEGIN_DECLS

/****************/
/* 2D detectors */
/****************/

typedef enum _HklBinocularsDetectorEnum
{
        HKL_BINOCULARS_DETECTOR_IMXPAD_S140 = 0,
        HKL_BINOCULARS_DETECTOR_XPAD_FLAT_CORRECTED,
        HKL_BINOCULARS_DETECTOR_IMXPAD_S70,
        HKL_BINOCULARS_DETECTOR_DECTRIS_EIGER1M,
        HKL_BINOCULARS_DETECTOR_UFXC,
        HKL_BINOCULARS_DETECTOR_MERLIN,
        HKL_BINOCULARS_DETECTOR_MERLIN_MEDIPIX_3RX_QUAD,
        HKL_BINOCULARS_DETECTOR_MERLIN_MEDIPIX_3RX_QUAD_512,
        HKL_BINOCULARS_DETECTOR_CIRPAD,
	HKL_BINOCULARS_DETECTOR_RIGAKU_XSPA_1M,
	HKL_BINOCULARS_DETECTOR_DECTRIS_PILATUS_CDTE_2M,
        /* Add new your detectors here */
        HKL_BINOCULARS_DETECTOR_NUM_DETECTORS,
        /* wip */
} HklBinocularsDetectorEnum;

HKLAPI extern int hkl_binoculars_detector_2d_number_of_detectors(void);

HKLAPI extern const char *hkl_binoculars_detector_2d_name_get(HklBinocularsDetectorEnum n);

HKLAPI extern void hkl_binoculars_detector_2d_shape_get(HklBinocularsDetectorEnum n,
                                                        int *width, int *height);

HKLAPI extern double *hkl_binoculars_detector_2d_coordinates_get(HklBinocularsDetectorEnum n);

HKLAPI extern void hkl_binoculars_detector_2d_coordinates_save(HklBinocularsDetectorEnum n,
                                                               const char *fname);

HKLAPI extern double *hkl_binoculars_detector_2d_flat_get(HklBinocularsDetectorEnum n);

HKLAPI extern double *hkl_binoculars_detector_2d_flat_load(HklBinocularsDetectorEnum n,
							   const char *filename);

HKLAPI extern uint8_t *hkl_binoculars_detector_2d_mask_get(HklBinocularsDetectorEnum n);

HKLAPI extern uint8_t *hkl_binoculars_detector_2d_mask_load(HklBinocularsDetectorEnum n,
                                                            const char *filename);

HKLAPI extern void hkl_binoculars_detector_2d_mask_save(HklBinocularsDetectorEnum n,
                                                        const char *fname);

HKLAPI extern void hkl_binoculars_detector_2d_sixs_calibration(HklBinocularsDetectorEnum n,
                                                               double *arr,
                                                               int width, int height,
                                                               int ix0, int iy0, double sdd,
                                                               double detrot, int normalize_flag);

HKLAPI extern void hkl_binoculars_detector_2d_pyfai_calibration(HklBinocularsDetectorEnum n,
								double *arr,
								int width, int height,
								double p1, double p2, double sdd,
								double rot1, double rot2, double rot3,
								int normalize_flag);

HKLAPI extern uint32_t *hkl_binoculars_detector_2d_fake_image_uint32(HklBinocularsDetectorEnum n,
                                                                     size_t *n_pixels);

HKLAPI extern int hkl_binoculars_detector_2d_img_load_into(HklBinocularsDetectorEnum n,
                                                           const char *filename,
                                                           void *buffer,
                                                           int n_buffer);

/***********/
/* Limits  */
/***********/

typedef struct _HklBinocularsAxisLimits HklBinocularsAxisLimits;

HKLAPI extern void hkl_binoculars_axis_limits_free(HklBinocularsAxisLimits *self);

HKLAPI extern HklBinocularsAxisLimits *hkl_binoculars_axis_limits_new(const ptrdiff_t *min,
                                                                      const ptrdiff_t *max);

/********/
/* Axis */
/********/

typedef struct _HklBinocularsAxis HklBinocularsAxis;

HKLAPI extern double *hkl_binoculars_axis_array(const HklBinocularsAxis *self);

/*********/
/* Space */
/*********/

typedef struct _HklBinocularsSpace HklBinocularsSpace;

typedef enum _HklBinocularsSurfaceOrientationEnum
{
        HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL = 0,
        HKL_BINOCULARS_SURFACE_ORIENTATION_HORIZONTAL,
        /* Add new your detectors here */
        HKL_BINOCULARS_SURFACE_ORIENTATION_NUM_ORIENTATION,
} HklBinocularsSurfaceOrientationEnum;


HKLAPI extern HklBinocularsSpace *hkl_binoculars_space_new(size_t n_indexes_0,
                                                           size_t n_axes);

HKLAPI extern void hkl_binoculars_space_free(HklBinocularsSpace *self);

HKLAPI extern void hkl_binoculars_space_fprintf(FILE *f, const HklBinocularsSpace *space);

/********/
/* Cube */
/********/

typedef  struct _HklBinocularsCube HklBinocularsCube;

HKLAPI extern void hkl_binoculars_cube_free(HklBinocularsCube *self);

HKLAPI extern HklBinocularsCube *hkl_binoculars_cube_new(size_t n_spaces,
                                                         const HklBinocularsSpace *const *spaces);

HKLAPI extern HklBinocularsCube *hkl_binoculars_cube_new_empty(void);

HKLAPI extern HklBinocularsCube *hkl_binoculars_cube_new_empty_from_cube(const HklBinocularsCube *src);

HKLAPI extern HklBinocularsCube *hkl_binoculars_cube_new_copy(const HklBinocularsCube *src);

HKLAPI extern HklBinocularsCube *hkl_binoculars_cube_new_from_file(const char *fname);

HKLAPI extern HklBinocularsCube *hkl_binoculars_cube_new_from_space(const HklBinocularsSpace *space);

HKLAPI extern HklBinocularsCube *hkl_binoculars_cube_new_merge(const HklBinocularsCube *cube1,
                                                               const HklBinocularsCube *cube2);

HKLAPI extern unsigned int hkl_binoculars_cube_cmp(const HklBinocularsCube *self,
                                                   const HklBinocularsCube *other);

HKLAPI extern void hkl_binoculars_cube_add_cube(HklBinocularsCube *self,
                                                const HklBinocularsCube *cube);

HKLAPI extern void hkl_binoculars_cube_add_space(HklBinocularsCube *self,
                                                 const HklBinocularsSpace *space);

HKLAPI extern int hkl_binoculars_cube_save_hdf5(const char *fn,
                                                const char *config,
                                                const HklBinocularsCube *self);

HKLAPI extern void hkl_binoculars_cube_merge_and_save_hdf5(const char *fn,
                                                           const char *config,
                                                           const char *fnames[],
                                                           size_t n_fnames);

HKLAPI extern void hkl_binoculars_cube_fprintf(FILE *f, const HklBinocularsCube *self);

/***************/
/* Projections */
/***************/

/* subprojection for QCustom */
/* q_scannumber / tth_scannumber  ( scannumber = numero du scan) (optional) */
/* q_timescan0 / tth_timescan0   (timescan0 = timestamp du 1er point du scan (constant pour 1 scan)) */
/* q_chi_phi (q, chi = arccos(qz/q), phi = arctan2(qy,qx)) */

/* Adding the polarization correction from fit2D */
/* dans un premier temps faire (1 - (epsilon (0, 1, 0) . kf) ^ 2) */
/* pola = "0.5 * (1.0 + cos(tth)**2 - factor * cos(2.0 * (chi + axis_offset)) * (1.0 - cos(tth)**2))" */

/* angles */

#define HKL_BINOCULARS_SPACE_ANGLES_DECL(image_t)			\
        void hkl_binoculars_space_angles_ ## image_t (HklBinocularsSpace *space, \
                                                      const HklGeometry *geometry, \
                                                      const image_t *image, \
                                                      size_t n_pixels,  \
                                                      double weight,    \
						      const double *flat, \
                                                      const double *pixels_coordinates, \
                                                      size_t pixels_coordinates_ndim, \
                                                      const size_t *pixels_coordinates_dims, \
                                                      const double *resolutions, \
                                                      size_t n_resolutions, \
                                                      const uint8_t *masked, \
                                                      const HklBinocularsAxisLimits **limits, \
                                                      size_t n_limits,	\
                                                      const char *sample_axis)

HKLAPI extern HKL_BINOCULARS_SPACE_ANGLES_DECL(double);
HKLAPI extern HKL_BINOCULARS_SPACE_ANGLES_DECL(int32_t);
HKLAPI extern HKL_BINOCULARS_SPACE_ANGLES_DECL(uint16_t);
HKLAPI extern HKL_BINOCULARS_SPACE_ANGLES_DECL(uint32_t);

/* qcustom */

typedef enum _HklBinocularsQCustomSubProjectionEnum
{
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_QZ = 0,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TTH_TIMESTAMP,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TIMESTAMP,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_TIMESTAMP,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIX_QX,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIY_QY,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHIZ_QZ,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_STEREO,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_DELTALAB_GAMMALAB_SAMPLEAXIS,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_X_Y_Z,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Y_Z_TIMESTAMP,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_QPAR_QPER,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPARS_QPER_TIMESTAMP,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_SAMPLEAXIS,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SAMPLEAXIS_TTH,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SAMPLEAXIS_TIMESTAMP,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_TIMESTAMP,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QZ_TIMESTAMP,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QY_QZ_TIMESTAMP,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_TTH_AZIMUTH,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TIMESCAN0,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_SCANNUMBER,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_TTH_SCANNUMBER,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIX_Q_THETAX,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIY_Q_THETAY,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_PHIZ_Q_THETAZ,
        /* Add new your subprojection in the same order than the haskell order here */
        HKL_BINOCULARS_QCUSTOM_NUM_SUBPROJECTIONS,
} HklBinocularsQCustomSubProjectionEnum;

#define HKL_BINOCULARS_SPACE_QCUSTOM_DECL(image_t)			\
        void hkl_binoculars_space_qcustom_ ## image_t (HklBinocularsSpace *space, \
                                                       const HklGeometry *geometry, \
                                                       const image_t *image, \
                                                       size_t n_pixels, \
                                                       double weight,   \
						       const double *flat, \
                                                       const double *pixels_coordinates, \
                                                       size_t pixels_coordinates_ndim, \
                                                       const size_t *pixels_coordinates_dims, \
                                                       const double *resolutions, \
                                                       size_t n_resolutions, \
                                                       const uint8_t *masked, \
                                                       HklBinocularsSurfaceOrientationEnum surf, \
                                                       const HklBinocularsAxisLimits **limits, \
                                                       size_t n_limits, \
                                                       double timestamp, \
                                                       double timescan0, \
                                                       int scannumber,  \
                                                       const HklBinocularsQCustomSubProjectionEnum subprojection, \
                                                       double uqx,      \
                                                       double uqy,      \
                                                       double uqz,      \
                                                       const char *sample_axis, \
                                                       bool do_polarisation_correction, \
						       double *image_threshold \
                )


HKLAPI extern HKL_BINOCULARS_SPACE_QCUSTOM_DECL(double);
HKLAPI extern HKL_BINOCULARS_SPACE_QCUSTOM_DECL(int32_t);
HKLAPI extern HKL_BINOCULARS_SPACE_QCUSTOM_DECL(uint16_t);
HKLAPI extern HKL_BINOCULARS_SPACE_QCUSTOM_DECL(uint32_t);

/* hkl */

#define HKL_BINOCULARS_SPACE_HKL_DECL(image_t)                          \
        void hkl_binoculars_space_hkl_ ## image_t (HklBinocularsSpace *space, \
                                                   const HklGeometry *geometry, \
                                                   const HklSample *sample, \
                                                   const image_t *image, \
                                                   size_t n_pixels,     \
                                                   double weight,       \
						   const double *flat,	\
                                                   const double *pixels_coordinates, \
                                                   size_t pixels_coordinates_ndim, \
                                                   const size_t *pixels_coordinates_dims, \
                                                   const double *resolutions, \
                                                   size_t n_resolutions, \
                                                   const uint8_t *masked, \
                                                   const HklBinocularsAxisLimits **limits, \
                                                   size_t n_limits,     \
                                                   bool do_polarisation_correction, \
						   double *image_threshold \
                )

HKLAPI extern HKL_BINOCULARS_SPACE_HKL_DECL(double);
HKLAPI extern HKL_BINOCULARS_SPACE_HKL_DECL(int32_t);
HKLAPI extern HKL_BINOCULARS_SPACE_HKL_DECL(uint16_t);
HKLAPI extern HKL_BINOCULARS_SPACE_HKL_DECL(uint32_t);

/* test */

#define HKL_BINOCULARS_SPACE_TEST_DECL(image_t)				\
        void hkl_binoculars_space_test_ ## image_t (HklBinocularsSpace *space, \
                                                    const HklGeometry *geometry, \
                                                    const HklSample *sample, \
                                                    const image_t *image, \
                                                    size_t n_pixels,    \
                                                    double weight,      \
						    const double *flat, \
                                                    const double *pixels_coordinates, \
                                                    size_t pixels_coordinates_ndim, \
                                                    const size_t *pixels_coordinates_dims, \
                                                    const double *resolutions, \
                                                    size_t n_resolutions, \
                                                    const uint8_t *masked, \
                                                    const HklBinocularsAxisLimits **limits, \
                                                    size_t n_limits,    \
                                                    bool do_polarisation_correction, \
						    double *image_threshold \
                )


HKLAPI extern HKL_BINOCULARS_SPACE_TEST_DECL(double);
HKLAPI extern HKL_BINOCULARS_SPACE_TEST_DECL(int32_t);
HKLAPI extern HKL_BINOCULARS_SPACE_TEST_DECL(uint16_t);
HKLAPI extern HKL_BINOCULARS_SPACE_TEST_DECL(uint32_t);


G_END_DECLS

#endif
