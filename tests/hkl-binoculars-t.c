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

#include <openssl/crypto.h>
#include <openssl/evp.h>
#include <openssl/sha.h>

#include "hkl-binoculars.h"
#include <tap/basic.h>
#include <tap/float.h>
#include <tap/hkl-tap.h>

#include <hkl-geometry-private.h>
#include <hkl-axis-private.h>
#include <hkl-binoculars-private.h>

static int check_sha256(const uint8_t *buffer, size_t length, const char* fingerprint, int n)
{
	int res = TRUE;

	OSSL_LIB_CTX *context = OSSL_LIB_CTX_new();
	uint8_t sha256[SHA256_DIGEST_LENGTH];
	size_t sha256_length;
	char buffer_fingerprint[SHA256_DIGEST_LENGTH*2+1];

	EVP_Q_digest(context, "SHA256", NULL, buffer, length, sha256, &sha256_length);
	for(int i=0; i<SHA256_DIGEST_LENGTH; ++i)
		snprintf(&buffer_fingerprint[2*i], 3, "%02x", sha256[i]);

	if (NULL == fingerprint){
		fprintf(stdout, "detector %d fingerpring: %s skipped\n", n, buffer_fingerprint);
		return res;
	}

	res &= DIAG(strcmp(fingerprint, buffer_fingerprint) == 0);
	if (res != TRUE)
		fprintf(stdout, "detector %d fingerpring: %s , expected: %s\n", n, buffer_fingerprint, fingerprint);

	OSSL_LIB_CTX_free(context);

	return res;
}

static int check_sha256_detector_array(int n, const uint8_t *buffer, size_t elem_size, const char *fingerprint)
{
	int res = TRUE;
	int width;
	int height;

	hkl_binoculars_detector_2d_shape_get(n, &width, &height);
	res &= DIAG(check_sha256(buffer, width * height * elem_size, fingerprint, n));

	return res;
}


static void coordinates_get(void)
{
        int res = TRUE;
	char *fingerprint[] = {
		"76a1c568b15e359b6c16c8c88bd738763554d80256dc44c41a81658636720d26",
		"5e3c73be48046c7e234cbd4bdffe54041c6bf04f18fdb37347577c68bb824fbf",
		"b58814cca765b3bbc7b1b526539b3e1fbe1d66aaff5fe8bb354163898971c26c",
		"f728c04d108698a9615c02b415b5e9b6e1de40ee1f55b61a3cfa79dd24a72f28",
		"a6a43c95847321fa29dbf6ac979845f987bcfa0982ca8513d415a871db3acf69",
		"07854d2fef297a06ba81685e660c332de36d5d18d546927d30daad6d7fda1541",
		"feced70203b04a1a25c0931b1f76f70af248991c31358b1b70fce2d80647966e",
		"5647f05ec18958947d32874eeb788fa396a05d0bab7c1b71f112ceb7e9b31eee",
/* CIRPAD */
#if __x86_64__ && __LP64__ /* amd64 */
                "0f3d88d8d4f3f6829d068b616b6d92b3fb91e6b039c78fa09bc0e447d5343dcd",
#elif __aarch64__ && __LP64__ /* arm64 */
		"939416e43163951cce69556a31aab15e1aa918f494118d3fed7ba9454c018aba",
#elif __arm__ && __ARM_EABI__ && __ARM_PCS_VFP /* armhf */
		"939416e43163951cce69556a31aab15e1aa918f494118d3fed7ba9454c018aba",
#elif __i386__ /* i386 */
		"1e82c811973083acda4c95fb12ebf8a872dc1f70edd6e5fd6371e46bfe96059d",
#elif __loongarch__ && __loongarch_lp64 && __loongarch_double_float  /* loong64 */
		NULL, /* TODO */
#elif __powerpc__ && __powerpc64__ && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__ /* ppc64el */
		"087f370764181cef79788b4c60240bf5127e45fca9ecc814dfbe3f2e90f765c5",
#elif __riscv && __riscv_xlen==64 /* riscv64 */
		NULL, /* TODO */
#elif __s390x__ /* s390x */
		"8e79155d882a5af83202bd8e5546ce7f64ffbe7f8cb665f21e2cbc9f77c811fe",
#endif
		"c5a68aa2d84bf11c4a1fb38a2830f6da1e6bcc75bccc87035e26519ee57ce35d",
		"0f1c88affde3e4fa2f46237056cc937c317966e1df522462301a383e737101f2",
	};

        for(int i=0; i<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++i){
                double *arr = hkl_binoculars_detector_2d_coordinates_get(i);
		if (NULL != arr){
			res &= DIAG(check_sha256_detector_array(i, (uint8_t *)(&arr[0]), sizeof(*arr), fingerprint[i]));
			free(arr);
		}
        }
	ok(res == TRUE, __func__);
}

static void coordinates_save(void)
{
        int res = TRUE;

        for(int i=0; i<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++i){
                char buffer[256];

                snprintf(buffer, ARRAY_SIZE(buffer), "/tmp/coordinates_%d.npy", i);
                hkl_binoculars_detector_2d_coordinates_save(i, buffer);
        }
	ok(res == TRUE, __func__);
}

/* static void io_img(void) */
/* { */
/*         int res = TRUE; */
/*         uint16_t *arr = NULL; */

/*         arr = hkl_binoculars_detector_2d_img_load(HKL_BINOCULARS_DETECTOR_RIGAKU_XSPA_1M, */
/*                                                   "../binoculars-ng/fluo_Ge_60sec_14b_single_000000.img"); */

/*         res &= NULL != arr; */

/*         if (NULL != arr) */
/*                 free(arr); */

/*	ok(res == TRUE, __func__); */
/* } */

static void mask_get(void)
{
        int res = TRUE;
	char *fingerprint[] = {
		"ae79534d175e8ebad19651a4dab71247d884466d6399c8641c22623a69f29b50",
		"911166d48b7a7a3fb8dfe07dc39b70f6478098bdc5a097616bd7ae99cbfee489",
		"3ae731393076cad3400b4ce9b17deadb3befa271040926ef1f6349fea068d596",
		"95190f41b0a9b1b45890a486a45bdbc3404284e149b5e72ce7dc89de85b2a735",
		"8d79f3966ca57c37fb086844e2536919235336676418fd08bbc2a99d19743f2a",
		"de2f256064a0af797747c2b97505dc0b9f3df0de4f489eac731c23ae9ca9cc31",
		"4c5d798882747c389cb83bf94dadfdd369d0955e658b246184feb4bdbc5557eb",
		"0f451b99cf4316ad57d46d4e399d11a1928790d3ec11a8790069b65a3d3d177c",
		"d37a5d7abcc9d895f232ec4be6ef49af7c77f133808e392576f0e6a96a2f8fcd",
		"fad1fd1073a50dd69fd93888376f857c2b03a410a005de4df3457f0eb968a91a",
		"ebfc99da3d9bfeafe3fdd474cb27f9a265f076315ff95f55bd37fc6c6bbfdf66",
	};

        for(int i=0; i<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++i){
                uint8_t *arr = hkl_binoculars_detector_2d_mask_get(i);
		if (NULL != arr){
			res &= DIAG(check_sha256_detector_array(i, arr, sizeof(*arr), fingerprint[i]));
			free(arr);
		}
        }
	ok(res == TRUE, __func__);
}

static void mask_save(void)
{
        int res = TRUE;

        for(int i=0; i<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++i){
                char buffer[256];

                snprintf(buffer, ARRAY_SIZE(buffer), "/tmp/mask_%d.npy", i);
                hkl_binoculars_detector_2d_mask_save(i, buffer);
        }
	ok(res == TRUE, __func__);
}

/* TODO */
/* static void mask_load(void) */
/* { */
/*         const char *fname = "/home/picca/tests-datas/binoculars/sixs/eiger/mask_nxs00007_20191105_15h01.npy"; */
/*         hkl_binoculars_detector_2d_mask_load(1, fname); */
/* } */

static void angles_projection(void)
{
        size_t n;
        int res = TRUE;
        HklFactory *factory = hkl_factory_get_by_name("ZAXIS", NULL);
        HklGeometry *geometry = hkl_factory_create_new_geometry(factory);

        for(n=0; n<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++n){
                size_t i;
                int height;
                int width;
                HklBinocularsCube *cube;
                HklBinocularsSpace *space;
                double *pixels_coordinates;
                uint8_t *mask;
		double *flat;
                ptrdiff_t min, max;
                HklBinocularsAxisLimits *delta_lims, *gamma_lims, *omega_lims;

                min = 0 / 0.05, max = 0.4 / 0.05;
                delta_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.4 / 0.05, max = 0.4 / 0.05;
                gamma_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.4 / 0.05, max = 0.4 / 0.05;
                omega_lims = hkl_binoculars_axis_limits_new(&min, &max);

                const HklBinocularsAxisLimits *limits[] = {delta_lims, gamma_lims, omega_lims};

                hkl_binoculars_detector_2d_shape_get(n, &width, &height);
                space = hkl_binoculars_space_new(width * height, 3);
                cube = hkl_binoculars_cube_new_empty();
                pixels_coordinates = hkl_binoculars_detector_2d_coordinates_get(n);
                mask = hkl_binoculars_detector_2d_mask_get(n);
		flat = hkl_binoculars_detector_2d_flat_get(n);

                for(i=0; i<3; ++i){
                        size_t img_size;
                        uint32_t *img = hkl_binoculars_detector_2d_fake_image_uint32(n, &img_size);
			const double weight = 1;
                        size_t pixels_coordinates_dims[] = {3, height, width};
                        const double resolutions[] = {1, 1, 1};
			const char *sampleaxis = "omega";

                        hkl_geometry_randomize(geometry);

                        hkl_binoculars_space_angles_uint32_t (space,
                                                              geometry,
                                                              img,
                                                              img_size,
                                                              weight,
							      flat,
                                                              pixels_coordinates,
                                                              ARRAY_SIZE(pixels_coordinates_dims),
                                                              pixels_coordinates_dims,
                                                              resolutions,
                                                              ARRAY_SIZE(resolutions),
                                                              mask,
                                                              limits,
                                                              ARRAY_SIZE(limits),
                                                              sampleaxis);

                        hkl_binoculars_cube_add_space(cube, space);

                        free(img);
                }

                free(mask);
                free(pixels_coordinates);
                hkl_binoculars_cube_free(cube);
                hkl_binoculars_space_free(space);
                hkl_binoculars_axis_limits_free(omega_lims);
                hkl_binoculars_axis_limits_free(gamma_lims);
                hkl_binoculars_axis_limits_free(delta_lims);
        }

        hkl_geometry_free(geometry);

        ok(res == TRUE, __func__);
}

static int qcustom_projection_sub(HklBinocularsQCustomSubProjectionEnum subprojection)
{
        size_t n;
        int res = TRUE;
        HklFactory *factory = hkl_factory_get_by_name("ZAXIS", NULL);
        HklGeometry *geometry = hkl_factory_create_new_geometry(factory);

        for(n=0; n<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++n){
                size_t i;
                int height;
                int width;
                HklBinocularsCube *cube;
                HklBinocularsSpace *space;
                double *pixels_coordinates;
                uint8_t *mask;
		double *flat;
                ptrdiff_t min, max;
                HklBinocularsAxisLimits *qx_lims, *qy_lims, *qz_lims;

                min = -0.45 / 0.05, max = 0.45 / 0.05;
                qx_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.45 / 0.05, max = 0.45 / 0.05;
                qy_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.45 / 0.05, max = 0.45 / 0.05;
                qz_lims = hkl_binoculars_axis_limits_new(&min, &max);

                const HklBinocularsAxisLimits *limits[] = {qx_lims, qy_lims, qz_lims};

                hkl_binoculars_detector_2d_shape_get(n, &width, &height);
                space = hkl_binoculars_space_new(width * height, 3);
                cube = hkl_binoculars_cube_new_empty();
                pixels_coordinates = hkl_binoculars_detector_2d_coordinates_get(n);
                mask = hkl_binoculars_detector_2d_mask_get(n);
		flat = hkl_binoculars_detector_2d_flat_get(n);

                for(i=0; i<3; ++i){
                        size_t img_size;
                        uint32_t *img = hkl_binoculars_detector_2d_fake_image_uint32(n, &img_size);
			const double weight = 1;
                        const size_t pixels_coordinates_dims[] = {3, height, width};
                        const double resolutions[] = {0.05, 0.05, 0.05};
			const double timestamp = 0;
			const double timestamp0 = 0;
			const int scannumber = 0;
			const double uqx = 0;
			const double uqy = 0;
			const double uqz = 0;
			const char *sampleaxis = "omega";
			const int do_polarisation_correction = 1;
			double dmask = 10000;

                        hkl_geometry_randomize(geometry);

                        hkl_binoculars_space_qcustom_uint32_t (space,
                                                               geometry,
                                                               img,
                                                               img_size,
                                                               weight,
							       flat,
                                                               pixels_coordinates,
                                                               ARRAY_SIZE(pixels_coordinates_dims),
                                                               pixels_coordinates_dims,
                                                               resolutions,
                                                               ARRAY_SIZE(resolutions),
                                                               mask,
                                                               HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL,
                                                               limits,
                                                               ARRAY_SIZE(limits),
                                                               timestamp,
                                                               timestamp0,
							       scannumber,
                                                               subprojection,
                                                               uqx, uqy, uqz,
                                                               sampleaxis,
                                                               do_polarisation_correction,
							       &dmask);

                        hkl_binoculars_cube_add_space(cube, space);

                        free(img);
                }

                free(mask);
                free(pixels_coordinates);
                hkl_binoculars_cube_free(cube);
                hkl_binoculars_space_free(space);
                hkl_binoculars_axis_limits_free(qz_lims);
                hkl_binoculars_axis_limits_free(qy_lims);
                hkl_binoculars_axis_limits_free(qx_lims);
        }

	hkl_geometry_free(geometry);

        return res;
}


static void qcustom_projection(void)
{
        int res = TRUE;
        int i;

        for(i=0; i<HKL_BINOCULARS_QCUSTOM_NUM_SUBPROJECTIONS; ++i) {
                res &= DIAG(qcustom_projection_sub(i));
        }

        ok(res == TRUE, __func__);
}

static void qparqper_projection(void)
{
        size_t n;
        int res = TRUE;
        HklFactory *factory = hkl_factory_get_by_name("ZAXIS", NULL);
        HklGeometry *geometry = hkl_factory_create_new_geometry(factory);

        for(n=0; n<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++n){
                size_t i;
                int height;
                int width;
                HklBinocularsCube *cube;
                HklBinocularsSpace *space;
                double *pixels_coordinates;
                uint8_t *mask;
		double *flat;
                ptrdiff_t min, max;
                HklBinocularsAxisLimits *qpar_lims, *qper_lims;

                min = 0 / 0.05, max = 0.4 / 0.05;
                qpar_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.4 / 0.05, max = 0.4 / 0.05;
                qper_lims = hkl_binoculars_axis_limits_new(&min, &max);

                const HklBinocularsAxisLimits *limits[] = {qpar_lims, qper_lims};

                hkl_binoculars_detector_2d_shape_get(n, &width, &height);
                space = hkl_binoculars_space_new(width * height, 2);
                cube = hkl_binoculars_cube_new_empty();
                pixels_coordinates = hkl_binoculars_detector_2d_coordinates_get(n);
                mask = hkl_binoculars_detector_2d_mask_get(n);
		flat = hkl_binoculars_detector_2d_flat_get(n);

                for(i=0; i<3; ++i){
                        size_t img_size;
                        uint32_t *img = hkl_binoculars_detector_2d_fake_image_uint32(n, &img_size);
			const double weight = 1;
                        size_t pixels_coordinates_dims[] = {2, height, width};
                        double resolutions[] = {0.05, 0.05};
			const double timestamp = 0;
			const double timestamp0 = 0;
			const int scannumber = 0;
			const double uqx = 0;
			const double uqy = 0;
			const double uqz = 0;
			const char *sampleaxis = NULL;
			const int do_polarisation_correction = 1;
			double dmask = 100000;

                        hkl_geometry_randomize(geometry);

                        hkl_binoculars_space_qcustom_uint32_t (space,
                                                               geometry,
                                                               img,
                                                               img_size,
                                                               weight,
							       flat,
                                                               pixels_coordinates,
                                                               ARRAY_SIZE(pixels_coordinates_dims),
                                                               pixels_coordinates_dims,
                                                               resolutions,
                                                               ARRAY_SIZE(resolutions),
                                                               mask,
                                                               HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL,
                                                               limits,
                                                               ARRAY_SIZE(limits),
                                                               timestamp,
                                                               timestamp0,
							       scannumber,
                                                               HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER,
                                                               uqx, uqy, uqz,
                                                               sampleaxis,
                                                               do_polarisation_correction,
							       &dmask);

                        hkl_binoculars_cube_add_space(cube, space);

                        free(img);
                }

                free(mask);
                free(pixels_coordinates);
                hkl_binoculars_cube_free(cube);
                hkl_binoculars_space_free(space);
                hkl_binoculars_axis_limits_free(qper_lims);
                hkl_binoculars_axis_limits_free(qpar_lims);
        }

        hkl_geometry_free(geometry);

        ok(res == TRUE, __func__);
}

static void qxqyqz_projection(void)
{
        size_t n;
        int res = TRUE;
        HklFactory *factory = hkl_factory_get_by_name("ZAXIS", NULL);
        HklGeometry *geometry = hkl_factory_create_new_geometry(factory);

        for(n=0; n<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++n){
                size_t i;
                int height;
                int width;
                HklBinocularsCube *cube;
                HklBinocularsSpace *space;
                double *pixels_coordinates;
                uint8_t *mask;
		double *flat;
                ptrdiff_t min, max;
                HklBinocularsAxisLimits *qx_lims, *qy_lims, *qz_lims;

                min = -0.45 / 0.05, max = 0.45 / 0.05;
                qx_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.45 / 0.05, max = 0.45 / 0.05;
                qy_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.45 / 0.05, max = 0.45 / 0.05;
                qz_lims = hkl_binoculars_axis_limits_new(&min, &max);

                const HklBinocularsAxisLimits *limits[] = {qx_lims, qy_lims, qz_lims};

                hkl_binoculars_detector_2d_shape_get(n, &width, &height);
                space = hkl_binoculars_space_new(width * height, 3);
                cube = hkl_binoculars_cube_new_empty();
                pixels_coordinates = hkl_binoculars_detector_2d_coordinates_get(n);
                mask = hkl_binoculars_detector_2d_mask_get(n);
		flat = hkl_binoculars_detector_2d_flat_get(n);

                for(i=0; i<3; ++i){
                        size_t img_size;
                        uint32_t *img = hkl_binoculars_detector_2d_fake_image_uint32(n, &img_size);
                        size_t pixels_coordinates_dims[] = {3, height, width};
                        double resolutions[] = {0.05, 0.05, 0.05};
			const double timestamp = 0;
			const double timestamp0 = 0;
			const int scannumber = 0;
			const double uqx = 0;
			const double uqy = 0;
			const double uqz = 0;
			const char *sampleaxis = NULL;
			const int do_polarisation_correction = 1;
			double dmask = 400000;

                        hkl_geometry_randomize(geometry);

                        hkl_binoculars_space_qcustom_uint32_t (space,
                                                               geometry,
                                                               img,
                                                               img_size,
                                                               1.0,
							       flat,
                                                               pixels_coordinates,
                                                               ARRAY_SIZE(pixels_coordinates_dims),
                                                               pixels_coordinates_dims,
                                                               resolutions,
                                                               ARRAY_SIZE(resolutions),
                                                               mask,
                                                               HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL,
                                                               limits,
                                                               ARRAY_SIZE(limits),
							       timestamp,
                                                               timestamp0,
							       scannumber,
                                                               HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_QZ,
                                                               uqx, uqy, uqz,
                                                               sampleaxis,
                                                               do_polarisation_correction,
							       &dmask);

                        hkl_binoculars_cube_add_space(cube, space);

                        free(img);
                }

                free(mask);
                free(pixels_coordinates);
                hkl_binoculars_cube_free(cube);
                hkl_binoculars_space_free(space);
                hkl_binoculars_axis_limits_free(qz_lims);
                hkl_binoculars_axis_limits_free(qy_lims);
                hkl_binoculars_axis_limits_free(qx_lims);
        }

	hkl_geometry_free(geometry);

        ok(res == TRUE, __func__);
}


static void hkl_projection(void)
{
        size_t n;
        int res = TRUE;
        HklFactory *factory = hkl_factory_get_by_name("ZAXIS", NULL);
        HklGeometry *geometry = hkl_factory_create_new_geometry(factory);

        /* prepare the sample */
        HklSample *sample = hkl_sample_new("hkl projection");
	HklLattice *lattice = hkl_lattice_new(2.556, 2.556, 2.556,
                                              90 * HKL_DEGTORAD,
                                              90 * HKL_DEGTORAD,
                                              120 * HKL_DEGTORAD,
                                              NULL);
	hkl_sample_lattice_set(sample, lattice);
	hkl_lattice_free(lattice);

        /* ux = -89.7639 */
        /* uy = 0.0252 */
        /* uz = 13.5010 */

        /* the geometry positions */

        static double axes[3][4] = {{0, 0, 0, 0},
                                    {10, 10, 10, 10},
                                    {30, 30, 30, 30}};

        /* the expected result for the three axes of the 1st detector */
        static ptrdiff_t imin[HKL_BINOCULARS_DETECTOR_NUM_DETECTORS][3] = {
		{-14,-2, 0},
		{-14,-2, 0},
		{-14,-2, 0},
		{-14,-2,-2},
		{-13, 0, 0},
		{-13, 0, 0},
		{-13,-1, 0},
		{-13,-1, 0},
		{-18, 0,-1},
		{-14,-2, -3},
		{-14,-8, -9},
	};

        static ptrdiff_t imax[HKL_BINOCULARS_DETECTOR_NUM_DETECTORS][3] = {
		{1, 34, 15},
		{1, 34, 19},
		{1, 34, 14},
		{1, 33, 15},
		{0, 33, 15},
		{0, 33, 15},
		{0, 33, 15},
		{0, 33, 15},
		{0, 33, 36},
		{1, 33, 15},
		{3, 34, 15},
	};

        for(n=0; n<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++n){
                size_t i;
                int height;
                int width;
                HklBinocularsCube *cube;
                HklBinocularsSpace *space;
                double *pixels_coordinates;
                uint8_t *mask;
		double *flat;
                HklBinocularsAxisLimits *h_lims, *k_lims, *l_lims;

                h_lims = hkl_binoculars_axis_limits_new(NULL, NULL);
                k_lims = hkl_binoculars_axis_limits_new(NULL, NULL);
                l_lims = hkl_binoculars_axis_limits_new(NULL, NULL);

                const HklBinocularsAxisLimits *limits[] = {h_lims, k_lims, l_lims};

                hkl_binoculars_detector_2d_shape_get(n, &width, &height);
                space = hkl_binoculars_space_new(width * height, 3);
                cube = hkl_binoculars_cube_new_empty();
                pixels_coordinates = hkl_binoculars_detector_2d_coordinates_get(n);
                hkl_binoculars_detector_2d_sixs_calibration(n, pixels_coordinates, width, height,
                                                            100, 100, 1,
                                                            0, 0);

                mask = hkl_binoculars_detector_2d_mask_get(n);
		flat = hkl_binoculars_detector_2d_flat_get(n);

                for(i=0; i<3; ++i){
			size_t img_size;
			uint32_t *img = hkl_binoculars_detector_2d_fake_image_uint32(n, &img_size);
			const double weight = 1;
			size_t pixels_coordinates_dims[] = {3, height, width};
			double resolutions[] = {0.05, 0.05, 0.05};
			const int do_polarisation_correction = 0;

                        IGNORE(hkl_geometry_axis_values_set(geometry, &axes[i][0], ARRAY_SIZE(axes[i]), HKL_UNIT_USER, NULL));

                        hkl_binoculars_space_hkl_uint32_t (space,
                                                           geometry,
                                                           sample,
                                                           img,
                                                           img_size,
                                                           weight,
							   flat,
                                                           pixels_coordinates,
                                                           ARRAY_SIZE(pixels_coordinates_dims),
                                                           pixels_coordinates_dims,
                                                           resolutions,
                                                           ARRAY_SIZE(resolutions),
                                                           mask,
                                                           limits,
                                                           ARRAY_SIZE(limits),
                                                           do_polarisation_correction,
							   NULL);
                        hkl_binoculars_cube_add_space(cube, space);

                        free(img);
                }

                for(i=0; i<ARRAY_SIZE(imax[n]); ++i){
                        res &= DIAG(imin[n][i] == darray_item(cube->axes, i).imin);
                        res &= DIAG(imax[n][i] == darray_item(cube->axes, i).imax);
                }

                if (res == FALSE){
                        fprintf(stdout, "\n");
                        hkl_binoculars_cube_fprintf(stdout, cube);
                }

                free(mask);
                free(pixels_coordinates);
                hkl_binoculars_cube_free(cube);
                hkl_binoculars_space_free(space);
                hkl_binoculars_axis_limits_free(l_lims);
                hkl_binoculars_axis_limits_free(k_lims);
                hkl_binoculars_axis_limits_free(h_lims);
        }

        hkl_sample_free(sample);
	hkl_geometry_free(geometry);

        ok(res == TRUE, __func__);
}

static void test_projection(void)
{
        size_t n;
        int res = TRUE;
        HklFactory *factory = hkl_factory_get_by_name("ZAXIS", NULL);
        HklGeometry *geometry = hkl_factory_create_new_geometry(factory);

        /* prepare the sample */
        HklSample *sample = hkl_sample_new("hkl projection");
	HklLattice *lattice = hkl_lattice_new(2.556, 2.556, 2.556,
                                              90 * HKL_DEGTORAD,
                                              90 * HKL_DEGTORAD,
                                              120 * HKL_DEGTORAD,
                                              NULL);
	hkl_sample_lattice_set(sample, lattice);
	hkl_lattice_free(lattice);

        /* ux = -89.7639 */
        /* uy = 0.0252 */
        /* uz = 13.5010 */

        /* the geometry positions */

        static double axes[3][4] = {{0, 0, 0, 0},
                                    {10, 10, 10, 10},
                                    {30, 30, 30, 30}};

        /* the expected result for the three axes of the 1st detector */
        static ptrdiff_t imin[HKL_BINOCULARS_DETECTOR_NUM_DETECTORS][3] = {
		{-14,-2, 0},
		{-14,-2, 0},
		{-14,-2, 0},
		{-14,-2,-2},
		{-13, 0, 0},
		{-13, 0, 0},
		{-13,-1, 0},
		{-13,-1, 0},
		{-18, 0,-1},
		{-14,-2, -3},
		{-14,-8, -9},
	};

        static ptrdiff_t imax[HKL_BINOCULARS_DETECTOR_NUM_DETECTORS][3] = {
		{1, 34, 15},
		{1, 34, 19},
		{1, 34, 14},
		{1, 33, 15},
		{0, 33, 15},
		{0, 33, 15},
		{0, 33, 15},
		{0, 33, 15},
		{0, 33, 36},
		{1, 33, 15},
		{3, 34, 15},
	};

        for(n=0; n<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++n){
                size_t i;
                int height;
                int width;
                HklBinocularsCube *cube, *cube2;
                HklBinocularsSpace *space;
                double *pixels_coordinates;
                double *pixels_coordinates_2;
                uint8_t *mask;
		double *flat;
                HklBinocularsAxisLimits *h_lims, *k_lims, *l_lims;

                h_lims = hkl_binoculars_axis_limits_new(NULL, NULL);
                k_lims = hkl_binoculars_axis_limits_new(NULL, NULL);
                l_lims = hkl_binoculars_axis_limits_new(NULL, NULL);

                const HklBinocularsAxisLimits *limits[] = {h_lims, k_lims, l_lims};

                hkl_binoculars_detector_2d_shape_get(n, &width, &height);
                space = hkl_binoculars_space_new(width * height, 3);
                cube = hkl_binoculars_cube_new_empty();
                cube2 = hkl_binoculars_cube_new_empty();
                pixels_coordinates = hkl_binoculars_detector_2d_coordinates_get(n);
                hkl_binoculars_detector_2d_sixs_calibration(n, pixels_coordinates, width, height,
                                                            100, 100, 1,
                                                            0, 1);

                pixels_coordinates_2 = hkl_binoculars_detector_2d_coordinates_get(n);
                hkl_binoculars_detector_2d_sixs_calibration(n, pixels_coordinates_2, width, height,
                                                            100, 100, 1,
                                                            0, 0);

                mask = hkl_binoculars_detector_2d_mask_get(n);
		flat = hkl_binoculars_detector_2d_flat_get(n);

                for(i=0; i<3; ++i){
                        size_t arr_size;
                        uint32_t *img = hkl_binoculars_detector_2d_fake_image_uint32(n, &arr_size);
                        size_t pixels_coordinates_dims[] = {3, height, width};
                        double resolutions[] = {0.05, 0.05, 0.05};
			const int do_polarisation_correction = 0;

                        IGNORE(hkl_geometry_axis_values_set(geometry, &axes[i][0], ARRAY_SIZE(axes[i]), HKL_UNIT_USER, NULL));

                        hkl_binoculars_space_hkl_uint32_t (space,
                                                           geometry,
                                                           sample,
                                                           img,
                                                           arr_size,
                                                           1.0,
							   flat,
                                                           pixels_coordinates,
                                                           ARRAY_SIZE(pixels_coordinates_dims),
                                                           pixels_coordinates_dims,
                                                           resolutions,
                                                           ARRAY_SIZE(resolutions),
                                                           mask,
                                                           limits,
                                                           ARRAY_SIZE(limits),
                                                           do_polarisation_correction,
							   NULL);
                        hkl_binoculars_cube_add_space(cube, space);

                        hkl_binoculars_space_test_uint32_t (space,
                                                            geometry,
                                                            sample,
                                                            img,
                                                            arr_size,
                                                            1.0,
							    flat,
                                                            pixels_coordinates_2,
                                                            ARRAY_SIZE(pixels_coordinates_dims),
                                                            pixels_coordinates_dims,
                                                            resolutions,
                                                            ARRAY_SIZE(resolutions),
                                                            mask,
                                                            limits,
                                                            ARRAY_SIZE(limits),
                                                            do_polarisation_correction,
							    NULL);
                        hkl_binoculars_cube_add_space(cube2, space);

                        free(img);
                }

                res &= DIAG(!hkl_binoculars_cube_cmp(cube, cube2));
                for(i=0; i<ARRAY_SIZE(imax[n]); ++i){
                        res &= DIAG(imin[n][i] == darray_item(cube->axes, i).imin);
                        res &= DIAG(imax[n][i] == darray_item(cube->axes, i).imax);
                        res &= DIAG(imin[n][i] == darray_item(cube2->axes, i).imin);
                        res &= DIAG(imax[n][i] == darray_item(cube2->axes, i).imax);
                }

                if (res == FALSE){
                        fprintf(stdout, "\n");
                        hkl_binoculars_cube_fprintf(stdout, cube);
                }


                free(mask);
                free(pixels_coordinates_2);
                free(pixels_coordinates);
                hkl_binoculars_cube_free(cube2);
                hkl_binoculars_cube_free(cube);
                hkl_binoculars_space_free(space);
                hkl_binoculars_axis_limits_free(l_lims);
                hkl_binoculars_axis_limits_free(k_lims);
                hkl_binoculars_axis_limits_free(h_lims);
        }

        hkl_sample_free(sample);
	hkl_geometry_free(geometry);

        ok(res == TRUE, __func__);
}

static void test_polarization(void)
{
	/* test the polarisation for the uhv geometry and the imxpad
	 * s140 detector */
        size_t n = HKL_BINOCULARS_DETECTOR_IMXPAD_S140;
        int res = TRUE;
        HklFactory *factory = hkl_factory_get_by_name("ZAXIS", NULL);
        HklGeometry *geometry = hkl_factory_create_new_geometry(factory);

        /* prepare the sample */
        HklSample *sample = hkl_sample_new("test polarization");
	HklLattice *lattice = hkl_lattice_new(2.556, 2.556, 2.556,
                                              90 * HKL_DEGTORAD,
                                              90 * HKL_DEGTORAD,
                                              120 * HKL_DEGTORAD,
                                              NULL);
	hkl_sample_lattice_set(sample, lattice);
	hkl_lattice_free(lattice);

        /* ux = -89.7639 */
        /* uy = 0.0252 */
        /* uz = 13.5010 */

        /* the geometry positions */

        static double axes[3][4] = {{0, 0, 0, 0},
                                    {10, 10, 10, 10},
                                    {30, 30, 30, 30}};

        /* the expected result for the three axes of the 1st detector */
        static ptrdiff_t imin[HKL_BINOCULARS_DETECTOR_NUM_DETECTORS][3] = {
		{-14,-2, 0},
		{-14,-2, 0},
		{-14,-2, 0},
		{-14,-2,-2},
		{-13, 0, 0},
		{-13, 0, 0},
		{-13,-1, 0},
		{-13,-1, 0},
		{-18, 0,-1},
		{-14,-2, -3},
	};

        static ptrdiff_t imax[HKL_BINOCULARS_DETECTOR_NUM_DETECTORS][3] = {
		{1, 34, 15},
		{1, 34, 19},
		{1, 34, 14},
		{1, 33, 15},
		{0, 33, 15},
		{0, 33, 15},
		{0, 33, 15},
		{0, 33, 15},
		{0, 33, 36},
		{1, 33, 15},
	};

	size_t i;
	int height;
	int width;
	HklBinocularsCube *cube;
	HklBinocularsSpace *space;
	double *pixels_coordinates;
	uint8_t *mask;
	double *flat;
	HklBinocularsAxisLimits *h_lims, *k_lims, *l_lims;

	h_lims = hkl_binoculars_axis_limits_new(NULL, NULL);
	k_lims = hkl_binoculars_axis_limits_new(NULL, NULL);
	l_lims = hkl_binoculars_axis_limits_new(NULL, NULL);

	const HklBinocularsAxisLimits *limits[] = {h_lims, k_lims, l_lims};

	hkl_binoculars_detector_2d_shape_get(n, &width, &height);
	space = hkl_binoculars_space_new(width * height, 3);
	cube = hkl_binoculars_cube_new_empty();
	pixels_coordinates = hkl_binoculars_detector_2d_coordinates_get(n);
	hkl_binoculars_detector_2d_sixs_calibration(n, pixels_coordinates, width, height,
						    100, 100, 1,
						    0, 0);

	mask = hkl_binoculars_detector_2d_mask_get(n);
	flat = hkl_binoculars_detector_2d_flat_get(n);

	for(i=0; i<3; ++i){
		size_t img_size;
		uint32_t *img = hkl_binoculars_detector_2d_fake_image_uint32(n, &img_size);
		const double weight = 1;
		size_t pixels_coordinates_dims[] = {3, height, width};
		double resolutions[] = {0.05, 0.05, 0.05};
		const int do_polarisation_correction = 0;

		IGNORE(hkl_geometry_axis_values_set(geometry, &axes[i][0], ARRAY_SIZE(axes[i]), HKL_UNIT_USER, NULL));

		hkl_binoculars_space_hkl_uint32_t (space,
						   geometry,
						   sample,
						   img,
						   img_size,
						   weight,
						   flat,
						   pixels_coordinates,
						   ARRAY_SIZE(pixels_coordinates_dims),
						   pixels_coordinates_dims,
						   resolutions,
						   ARRAY_SIZE(resolutions),
						   mask,
						   limits,
						   ARRAY_SIZE(limits),
						   do_polarisation_correction,
						   NULL);
		hkl_binoculars_cube_add_space(cube, space);

		free(img);
	}

	for(i=0; i<ARRAY_SIZE(imax[n]); ++i){
		res &= DIAG(imin[n][i] == darray_item(cube->axes, i).imin);
		res &= DIAG(imax[n][i] == darray_item(cube->axes, i).imax);
	}

	if (res == FALSE){
		fprintf(stdout, "\n");
		hkl_binoculars_cube_fprintf(stdout, cube);
	}

	free(mask);
	free(pixels_coordinates);
	hkl_binoculars_cube_free(cube);
	hkl_binoculars_space_free(space);
	hkl_binoculars_axis_limits_free(l_lims);
	hkl_binoculars_axis_limits_free(k_lims);
	hkl_binoculars_axis_limits_free(h_lims);

        hkl_sample_free(sample);
	hkl_geometry_free(geometry);

        ok(res == TRUE, __func__);
}


int main(void)
{
	plan(11);

	coordinates_get();
        coordinates_save();
//        io_img();
	mask_get();
        mask_save();
        angles_projection();
        qcustom_projection();
        qparqper_projection();
        qxqyqz_projection();
        hkl_projection();
        test_projection();
	test_polarization();

	return 0;
}
