#!/usr/bin/env python3
"""
This file is part of the hkl library.

The hkl library is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

The hkl library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.

Copyright (C) 2012-2013, 2023, 2024 Synchrotron SOLEIL
                        L'Orme des Merisiers Saint-Aubin
                        BP 48 91192 GIF-sur-YVETTE CEDEX
Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
"""

import math
import unittest
from gi.repository import GLib
import gi

gi.require_version("Hkl", "5.0")
from gi.repository import Hkl


class TestAPI(unittest.TestCase):
    """Test all the Hkl API, if something brakes here it means that API
    has changed !!!
    """

    def test_defines(self):
        self.assertTrue((type(Hkl.VERSION)) == str)

    def test_vector_api(self):
        """
        enforce the Vector api
        """
        v = Hkl.Vector()
        self.assertTrue(type(v) == Hkl.Vector)
        self.assertTrue(type(v.data) == list)
        self.assertTrue(3 == len(v.data))

        self.assertTrue(v.data == [0.0, 0.0, 0.0])
        v.init(1, 2, 3)
        self.assertTrue(v.data == [1.0, 2.0, 3.0])

        del v

    def test_quaternion_api(self):
        """
        enforce the Vector api
        """
        q = Hkl.Quaternion()
        self.assertTrue(type(q) == Hkl.Quaternion)
        self.assertTrue(type(q.data) == list)
        self.assertTrue(4 == len(q.data))

        del q

    def test_factory_api(self):
        """
        enforce the Factory API
        """
        # factories dict <name, Factory>
        factories = Hkl.factories()
        for key, factory in factories.items():
            self.assertTrue(type(key) == str)
            self.assertTrue(type(factory) == Hkl.Factory)

            # create all the geometry and engines
            geometry = factory.create_new_geometry()
            self.assertTrue(type(geometry) == Hkl.Geometry)
            engines = factory.create_new_engine_list()
            self.assertTrue(type(engines) == Hkl.EngineList)

        del factories

    def test_detector_api(self):
        """
        enforce the detector API
        """

        # create an 0D HklDetector
        detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))
        self.assertTrue(type(detector) is Hkl.Detector)

        del detector

    def test_geometry_api(self):
        """
        enforce the geometry API
        """

        # get the config for a given geometry and create the
        # corresponding HklGeometry
        factory = Hkl.factories()["K6C"]
        geometry = factory.create_new_geometry()
        detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))
        sample = Hkl.Sample.new("toto")

        # source access
        wavelength = 1.0
        geometry.wavelength_set(wavelength, Hkl.UnitEnum.USER)
        self.assertTrue(wavelength == geometry.wavelength_get(Hkl.UnitEnum.USER))  # noqa

        # set the geometry axes values
        values_w = [0, 30, 0, 0, 0, 60]
        geometry.axis_values_set(values_w, Hkl.UnitEnum.USER)
        values_r = geometry.axis_values_get(Hkl.UnitEnum.USER)

        # check that the read and write values of the geometry are
        # almost equals
        for r, w in zip(values_w, values_r):
            self.assertAlmostEqual(r, w)

        # check that we can access the axes
        axis_names = geometry.axis_names_get()
        for name in axis_names:
            axis = geometry.axis_get(name)
            axis.min_max_set(0, math.radians(180), Hkl.UnitEnum.USER)
            v = axis.axis_v_get()
            q = axis.quaternion_get()
            geometry.axis_set(name, axis)

        # check xxx_rotation_get
        q = geometry.sample_rotation_get(sample)
        m = q.to_matrix()
        self.assertTrue(type(m) == Hkl.Matrix)
        self.assertTrue(type(q) == Hkl.Quaternion)

        q = geometry.detector_rotation_get(detector)
        m = q.to_matrix()
        self.assertTrue(type(m) == Hkl.Matrix)
        self.assertTrue(type(q) == Hkl.Quaternion)

        # check ki/kf _get
        ki = geometry.ki_get()
        self.assertTrue(type(ki) == Hkl.Vector)
        kf = geometry.kf_get(detector)
        self.assertTrue(type(kf) == Hkl.Vector)

        del sample
        del detector
        del geometry

    def test_engine_api(self):
        """
        enforce the HklEngine API
        """

        detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))

        factory = Hkl.factories()["K6C"]
        geometry = factory.create_new_geometry()
        values_w = [0.0, 30.0, 0.0, 0.0, 0.0, 60.0]
        geometry.axis_values_set(values_w, Hkl.UnitEnum.USER)

        sample = Hkl.Sample.new("toto")
        lattice = sample.lattice_get()
        lattice.set(1.54, 1.54, 1.54, 90, 90, 90, Hkl.UnitEnum.USER)
        sample.lattice_set(lattice)

        # compute all the pseudo axes managed by all engines
        engines = factory.create_new_engine_list()
        engines.init(geometry, detector, sample)
        engines.get()

        # get the hkl engine and do a computation
        hkl = engines.engine_get_by_name("hkl")
        values = hkl.pseudo_axis_values_get(Hkl.UnitEnum.USER)

        # check for all modes
        for mode in hkl.modes_names_get():
            self.assertTrue(type(mode) is str)

        # set the hkl engine and get the results
        for _ in range(100):
            try:
                solutions = hkl.pseudo_axis_values_set(values, Hkl.UnitEnum.USER)
                self.assertTrue(type(solutions) is Hkl.GeometryList)
                for item in solutions.items():
                    self.assertTrue(type(item) is Hkl.GeometryListItem)
                    self.assertTrue(type(item.geometry_get()) is Hkl.Geometry)
                values[1] += 0.01
            except GLib.GError as err:
                print(values, err)

        # check that all the values computed are reachable
        for engine in engines.engines_get():
            self.assertTrue(type(engine) is Hkl.Engine)
            self.assertTrue(type(engine.name_get()) is str)
            self.assertTrue(type(engine.pseudo_axis_names_get()) is list)
            self.assertTrue(type(engine.modes_names_get()) is list)
            self.assertTrue(len(engine.modes_names_get()))
            for mode in engine.modes_names_get():
                self.assertTrue(type(mode) is str)
            values = engine.pseudo_axis_values_get(Hkl.UnitEnum.USER)
            self.assertTrue(type(values) is list)
            for value in values:
                self.assertTrue(type(value) is float)

        # check that all engine parameters and axes are reachables
        for engine in engines.engines_get():
            for mode in engine.modes_names_get():
                engine.current_mode_set(mode)

                parameters = engine.parameters_names_get()
                self.assertTrue(type(parameters) is list)
                [self.assertTrue(type(_) is str) for _ in parameters]
                # all together
                values = engine.parameters_values_get(Hkl.UnitEnum.USER)
                [self.assertTrue(type(_) is float) for _ in values]
                engine.parameters_values_set(values, Hkl.UnitEnum.USER)
                # one by one
                for parameter in parameters:
                    p = engine.parameter_get(parameter)
                    self.assertTrue(type(p.description_get()) is str)

                # check that parameters are writable.
                values = engine.parameters_values_get(Hkl.UnitEnum.USER)
                values = [1.0] * len(values)
                engine.parameters_values_set(values, Hkl.UnitEnum.USER)
                [
                    self.assertTrue(ref == v)
                    for ref, v in zip(
                        values, engine.parameters_values_get(Hkl.UnitEnum.USER)
                    )
                ]

                axes_r = engine.axis_names_get(Hkl.EngineAxisNamesGet.READ)
                self.assertTrue(type(axes_r) is list)
                [self.assertTrue(type(_) is str) for _ in axes_r]
                axes_w = engine.axis_names_get(Hkl.EngineAxisNamesGet.WRITE)
                self.assertTrue(type(axes_w) is list)
                [self.assertTrue(type(_) is str) for _ in axes_w]

        # check all the capabilities
        for engine in engines.engines_get():
            capabilities = engine.capabilities_get()
            self.assertTrue(capabilities & Hkl.EngineCapabilities.READABLE)
            if engine.name_get() not in ["incidence", "emergence"]:
                self.assertTrue(capabilities & Hkl.EngineCapabilities.WRITABLE)
            if engine.name_get() == "psi":
                self.assertTrue(capabilities & Hkl.EngineCapabilities.INITIALIZABLE)

        # check initialized_get/set
        for engine in engines.engines_get():
            initialized = engine.initialized_get()
            capabilities = engine.capabilities_get()
            if capabilities & Hkl.EngineCapabilities.INITIALIZABLE:
                engine.initialized_set(False)
                self.assertTrue(False == engine.initialized_get())
                engine.initialized_set(True)
                self.assertTrue(True == engine.initialized_get())

        # check all the dependencies
        for engine in engines.engines_get():
            dependencies = engine.dependencies_get()
            self.assertTrue(dependencies & Hkl.EngineDependencies.AXES)

    def test_engine_list_api(self):
        factories = Hkl.factories()
        for key, factory in factories.items():
            # print("diffractometer geometry factory = " + str(key))
            engines = factory.create_new_engine_list()

            # check the parameters
            names = engines.parameters_names_get()
            self.assertTrue(type(names) is list)
            [self.assertTrue(type(_) is str) for _ in names]

            # get/set the parameters
            for name in names:
                p = engines.parameter_get(name)
                self.assertTrue(type(p) is Hkl.Parameter)
                engines.parameter_set(name, p)

            # check that we can get/set the parameters values
            values = engines.parameters_values_get(Hkl.UnitEnum.USER)
            [self.assertTrue(type(_) is float) for _ in values]
            engines.parameters_values_set(values, Hkl.UnitEnum.USER)

    @unittest.skip("for testing figures")
    def test_doc_example(self):
        # execfile("../../Documentation/sphinx/source/bindings/python.py")
        execfile("../../Documentation/sphinx/source/pyplots/trajectory_simple.py")
        execfile("../../Documentation/sphinx/source/pyplots/trajectory_full.py")

        self.assertTrue(False)

    def test_lattice_api(self):
        lattice = Hkl.Lattice.new(
            1.54, 1.54, 1.54, math.radians(90.0), math.radians(90.0), math.radians(90.0)
        )
        lattice2 = lattice.copy()

        # check all the accessors
        a = lattice.a_get()
        b = lattice.b_get()
        c = lattice.c_get()
        alpha = lattice.alpha_get()
        beta = lattice.beta_get()
        gamma = lattice.gamma_get()

        lattice.a_set(a)
        lattice.b_set(b)
        lattice.c_set(c)
        lattice.alpha_set(alpha)
        lattice.beta_set(beta)
        lattice.gamma_set(gamma)

        # change the lattice parameter by expanding the tuple from
        # the get method. the lattice should not change.
        a, b, c, alpha, beta, gamma = lattice.get(Hkl.UnitEnum.DEFAULT)
        lattice.set(a, b, c, alpha, beta, gamma, Hkl.UnitEnum.DEFAULT)

        # now change the lattice parameter
        lattice.set(1, 2, 3, 90, 90, 90, Hkl.UnitEnum.USER)

        # this new lattice is different from the one in the sample
        self.assertTrue(
            lattice.get(Hkl.UnitEnum.DEFAULT) != lattice2.get(Hkl.UnitEnum.DEFAULT)
        )

        del lattice2
        del lattice

    def test_sample_api(self):
        """
        enforce the HklSample API
        """

        # create a sample
        sample = Hkl.Sample.new("toto")
        self.assertTrue(sample.name_get() == "toto")

        # check that the copy constructor is working
        copy = sample.copy()
        self.assertTrue(copy.name_get() == sample.name_get())
        # we can change the name of the copy without affecting the original
        copy.name_set("titi")
        self.assertTrue(copy.name_get() != sample.name_get())

        # set the lattice parameters
        lattice = Hkl.Lattice.new(
            1.54, 1.54, 1.54, math.radians(90.0), math.radians(90.0), math.radians(90.0)
        )
        sample.lattice_set(lattice)

        # change the lattice parameter by expanding the tuple from
        # the get method. the lattice should not change.
        a, b, c, alpha, beta, gamma = lattice.get(Hkl.UnitEnum.DEFAULT)
        lattice.set(a, b, c, alpha, beta, gamma, Hkl.UnitEnum.DEFAULT)

        # this new lattice is identical to the one from the sample
        v = lattice.get(Hkl.UnitEnum.DEFAULT)
        self.assertTrue(v == sample.lattice_get().get(Hkl.UnitEnum.DEFAULT))

        # now change the lattice parameter
        lattice.set(1, 2, 3, 90, 90, 90, Hkl.UnitEnum.USER)

        # this new lattice is different from the one in the sample
        v = lattice.get(Hkl.UnitEnum.DEFAULT)
        self.assertTrue(v != sample.lattice_get().get(Hkl.UnitEnum.DEFAULT))

        # gives access to the ux, uy, uz part
        ux = sample.ux_get()
        uy = sample.uy_get()
        uz = sample.uz_get()
        sample.ux_set(ux)
        sample.uy_set(uy)
        sample.uz_set(uz)

        # read and write the U matrix
        U = sample.U_get()
        UB = sample.UB_get()
        sample.UB_set(UB)

        # get the reciprocal lattice
        reciprocal = lattice.copy()
        lattice.reciprocal(reciprocal)

        # get the lattice volume
        lattice.volume_get()

        del sample
        del lattice

    def test_reflection_api(self):
        detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))

        factory = Hkl.factories()["K6C"]
        geometry = factory.create_new_geometry()
        values_w = [0.0, 30.0, 0.0, 0.0, 0.0, 60.0]
        geometry.axis_values_set(values_w, Hkl.UnitEnum.USER)

        sample = Hkl.Sample.new("toto")

        # add reflection
        r1 = sample.add_reflection(geometry, detector, 1, 1, 1)
        r2 = sample.add_reflection(geometry, detector, 1, 1, 1)

        # get the hkl part
        self.assertTrue(r2.hkl_get() == (1.0, 1.0, 1.0))
        r2.hkl_set(1, 0, 1)
        self.assertTrue(r2.hkl_get() == (1.0, 0.0, 1.0))

        # get the flag part
        flag = r1.flag_get()
        r1.flag_set(flag)

        # get the geometry part
        g = r1.geometry_get()
        r1.geometry_set(g)

        # compute the angles
        sample.get_reflection_measured_angle(r1, r2)
        sample.get_reflection_theoretical_angle(r1, r2)

        # remove all the reflections
        reflections = sample.reflections_get()
        for reflection in reflections:
            sample.del_reflection(reflection)

        del reflections
        del sample


class TestCDI322(unittest.TestCase):
    """Tests specific to the NSLS-II CDI 3-2-2 diffractometer geometry.

    Axes (in order): mu, omega, chi, omega2, chi2, gamma, delta
    Engines: hkl, q2, qper_qpar, tth2, incidence, emergence
    """

    FACTORY_NAME = "CDI-3-2-2"
    # cubic silicon-like sample, a = 1.54 Å — identical to the CU lattice
    # used throughout the other C tests
    LATTICE_A = 1.54
    WAVELENGTH = 1.54  # Å

    def _make_geometry_and_engines(self, axis_values=None):
        """Return (geometry, detector, sample, engines) for CDI-3-2-2."""
        factory = Hkl.factories()[self.FACTORY_NAME]
        geometry = factory.create_new_geometry()
        geometry.wavelength_set(self.WAVELENGTH, Hkl.UnitEnum.USER)
        if axis_values is not None:
            geometry.axis_values_set(axis_values, Hkl.UnitEnum.USER)
        detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))
        sample = Hkl.Sample.new("test")
        lattice = sample.lattice_get()
        lattice.set(
            self.LATTICE_A,
            self.LATTICE_A,
            self.LATTICE_A,
            90,
            90,
            90,
            Hkl.UnitEnum.USER,
        )
        sample.lattice_set(lattice)
        engines = factory.create_new_engine_list()
        engines.init(geometry, detector, sample)
        return geometry, detector, sample, engines

    # ------------------------------------------------------------------
    # Factory / geometry structure
    # ------------------------------------------------------------------

    def test_factory_exists(self):
        """CDI-3-2-2 must be present in the factory registry."""
        factories = Hkl.factories()
        self.assertIn(self.FACTORY_NAME, factories)

    def test_axis_names(self):
        """Geometry must expose the seven expected axes in the right order."""
        factory = Hkl.factories()[self.FACTORY_NAME]
        geometry = factory.create_new_geometry()
        names = list(geometry.axis_names_get())
        self.assertEqual(
            names, ["mu", "omega", "chi", "omega2", "chi2", "gamma", "delta"]
        )

    def test_engine_names(self):
        """Engine list must contain exactly the six expected engines."""
        factory = Hkl.factories()[self.FACTORY_NAME]
        engines = factory.create_new_engine_list()
        names = {e.name_get() for e in engines.engines_get()}
        self.assertEqual(
            names, {"hkl", "q2", "qper_qpar", "tth2", "incidence", "emergence"}
        )

    # ------------------------------------------------------------------
    # hkl engine
    # ------------------------------------------------------------------

    def test_hkl_engine_mode(self):
        """hkl engine must have exactly one mode: lifting_detector_omega."""
        _, _, _, engines = self._make_geometry_and_engines([0, 30, 0, 0, 0, 0, 60])
        engine = engines.engine_get_by_name("hkl")
        modes = list(engine.modes_names_get())
        self.assertEqual(modes, ["lifting_detector_omega"])

    def test_hkl_engine_forward(self):
        """Forward calculation: (mu=0, omega=30, chi=0, omega2=0, chi2=0, gamma=0, delta=60)
        with cubic a=1.54 Å at λ=1.54 Å must give (h, k, l) = (0, 0, 1)."""
        # mu=0, omega=30, chi=0, omega2=0, chi2=0, gamma=0, delta=60
        *_rest, engines = self._make_geometry_and_engines([0, 30, 0, 0, 0, 0, 60])
        engines.get()
        engine = engines.engine_get_by_name("hkl")
        h, k, l = engine.pseudo_axis_values_get(Hkl.UnitEnum.USER)
        self.assertAlmostEqual(h, 0, places=5)
        self.assertAlmostEqual(k, 0, places=5)
        self.assertAlmostEqual(l, 1, places=5)

    def test_hkl_engine_roundtrip(self):
        """Setting (h,k,l) = (0,0,1) must yield a geometry that reproduces it."""
        _geometry, detector, sample, engines = self._make_geometry_and_engines(
            [0, 30, 0, 0, 0, 0, 60]
        )
        engine = engines.engine_get_by_name("hkl")
        solutions = engine.pseudo_axis_values_set([0, 0, 1], Hkl.UnitEnum.USER)
        self.assertIsNotNone(solutions)
        items = solutions.items()
        self.assertGreater(len(items), 0)
        # verify each solution round-trips
        for item in items:
            geom = item.geometry_get()
            engines2 = Hkl.factories()[self.FACTORY_NAME].create_new_engine_list()
            engines2.init(geom, detector, sample)
            engines2.get()
            h, k, l = engines2.engine_get_by_name("hkl").pseudo_axis_values_get(
                Hkl.UnitEnum.USER
            )
            self.assertAlmostEqual(h, 0, places=4)
            self.assertAlmostEqual(k, 0, places=4)
            self.assertAlmostEqual(l, 1, places=4)

    def test_hkl_write_axes(self):
        """hkl engine in lifting_detector_omega must write omega, gamma, delta only."""
        _, _, _, engines = self._make_geometry_and_engines([0, 30, 0, 0, 0, 0, 60])
        engine = engines.engine_get_by_name("hkl")
        write_axes = set(engine.axis_names_get(Hkl.EngineAxisNamesGet.WRITE))
        self.assertEqual(write_axes, {"omega", "gamma", "delta"})

    # ------------------------------------------------------------------
    # tth2 engine
    # ------------------------------------------------------------------

    def test_tth2_engine_forward(self):
        """tth2 forward: delta=60°, gamma=0° → tth = 60° (π/3), alpha = 0."""
        *_rest, engines = self._make_geometry_and_engines([0, 0, 0, 0, 0, 0, 60])
        engines.get()
        engine = engines.engine_get_by_name("tth2")
        tth, alpha = engine.pseudo_axis_values_get(Hkl.UnitEnum.USER)
        self.assertAlmostEqual(tth, 60, places=5)
        self.assertAlmostEqual(alpha, 0, places=5)

    def test_tth2_engine_roundtrip(self):
        """tth2 set/get round-trip over a range of (tth, alpha) values."""
        _geometry, detector, sample, engines = self._make_geometry_and_engines(
            [0, 0, 0, 0, 0, 0, 60]
        )
        engine = engines.engine_get_by_name("tth2")
        for tth_deg in [30, 45, 60, 90]:
            for alpha_deg in [-30, 0, 30]:
                solutions = engine.pseudo_axis_values_set(
                    [tth_deg, alpha_deg], Hkl.UnitEnum.USER
                )
                if solutions is None:
                    continue
                item = solutions.items()[0]
                geom2 = item.geometry_get()
                engines2 = Hkl.factories()[self.FACTORY_NAME].create_new_engine_list()
                engines2.init(geom2, detector, sample)
                engines2.get()
                tth2, alpha2 = engines2.engine_get_by_name(
                    "tth2"
                ).pseudo_axis_values_get(Hkl.UnitEnum.USER)
                self.assertAlmostEqual(tth2, tth_deg, places=4)
                self.assertAlmostEqual(alpha2, alpha_deg, places=4)

    # ------------------------------------------------------------------
    # q2 engine
    # ------------------------------------------------------------------

    def test_q2_engine_forward(self):
        """q2 forward: delta=60°, gamma=0° at λ=1.54 Å.
        q = (4π/λ) sin(tth/2) = (4π/1.54) sin(30°) ≈ 4.0785 Å⁻¹."""
        *_rest, engines = self._make_geometry_and_engines([0, 0, 0, 0, 0, 0, 60])
        engines.get()
        engine = engines.engine_get_by_name("q2")
        q, alpha = engine.pseudo_axis_values_get(Hkl.UnitEnum.USER)
        expected_q = 4 * math.pi / self.WAVELENGTH * math.sin(math.radians(30))
        self.assertAlmostEqual(q, expected_q, places=4)
        self.assertAlmostEqual(alpha, 0, places=5)

    def test_q2_engine_roundtrip(self):
        """q2 set/get round-trip over a range of (q, alpha) values."""
        _geometry, detector, sample, engines = self._make_geometry_and_engines(
            [0, 0, 0, 0, 0, 0, 60]
        )
        engine = engines.engine_get_by_name("q2")
        qmax = 4 * math.pi / self.WAVELENGTH
        for q in [qmax * 0.2, qmax * 0.5, qmax * 0.8]:
            for alpha_deg in [-20, 0, 20]:
                solutions = engine.pseudo_axis_values_set(
                    [q, alpha_deg], Hkl.UnitEnum.USER
                )
                if solutions is None:
                    continue
                item = solutions.items()[0]
                geom2 = item.geometry_get()
                engines2 = Hkl.factories()[self.FACTORY_NAME].create_new_engine_list()
                engines2.init(geom2, detector, sample)
                engines2.get()
                q2, alpha2 = engines2.engine_get_by_name("q2").pseudo_axis_values_get(
                    Hkl.UnitEnum.USER
                )
                self.assertAlmostEqual(q2, q, places=4)
                self.assertAlmostEqual(alpha2, alpha_deg, places=4)

    # ------------------------------------------------------------------
    # incidence / emergence (read-only engines)
    # ------------------------------------------------------------------

    def test_incidence_engine_readonly(self):
        """incidence engine must be read-only (not writable)."""
        _, _, _, engines = self._make_geometry_and_engines([0, 0, 0, 0, 0, 0, 60])
        engine = engines.engine_get_by_name("incidence")
        caps = engine.capabilities_get()
        self.assertTrue(caps & Hkl.EngineCapabilities.READABLE)
        self.assertFalse(caps & Hkl.EngineCapabilities.WRITABLE)

    def test_emergence_engine_readonly(self):
        """emergence engine must be read-only (not writable)."""
        _, _, _, engines = self._make_geometry_and_engines([0, 0, 0, 0, 0, 0, 60])
        engine = engines.engine_get_by_name("emergence")
        caps = engine.capabilities_get()
        self.assertTrue(caps & Hkl.EngineCapabilities.READABLE)
        self.assertFalse(caps & Hkl.EngineCapabilities.WRITABLE)

    def test_incidence_all_zero(self):
        """With all sample axes at 0° and surface normal along Y, incidence = 0°."""
        *_rest, engines = self._make_geometry_and_engines([0, 0, 0, 0, 0, 0, 0])
        engines.get()
        engine = engines.engine_get_by_name("incidence")
        incidence, _azimuth = engine.pseudo_axis_values_get(Hkl.UnitEnum.USER)
        self.assertAlmostEqual(incidence, 0, places=5)

    def test_emergence_uses_detector_axes(self):
        """emergence must depend on detector axes: changing delta must change emergence."""
        _, _, _, engines_a = self._make_geometry_and_engines([0, 0, 0, 0, 0, 0, 0])
        _, _, _, engines_b = self._make_geometry_and_engines([0, 0, 0, 0, 0, 0, 60])
        engines_a.get()
        engines_b.get()
        em_a, _ = engines_a.engine_get_by_name("emergence").pseudo_axis_values_get(
            Hkl.UnitEnum.USER
        )
        em_b, _ = engines_b.engine_get_by_name("emergence").pseudo_axis_values_get(
            Hkl.UnitEnum.USER
        )
        self.assertNotAlmostEqual(em_a, em_b, places=3)

    def test_incidence_independent_of_detector(self):
        """incidence must NOT depend on detector axes (gamma/delta)."""
        _, _, _, engines_a = self._make_geometry_and_engines([0, 10, 5, 0, 0, 0, 0])
        _, _, _, engines_b = self._make_geometry_and_engines([0, 10, 5, 0, 0, 30, 60])
        engines_a.get()
        engines_b.get()
        inc_a, _ = engines_a.engine_get_by_name("incidence").pseudo_axis_values_get(
            Hkl.UnitEnum.USER
        )
        inc_b, _ = engines_b.engine_get_by_name("incidence").pseudo_axis_values_get(
            Hkl.UnitEnum.USER
        )
        self.assertAlmostEqual(inc_a, inc_b, places=10)


if __name__ == "__main__":
    unittest.main(verbosity=2)
