"""Regression tests for the matrix and its independent receipt consumer.

Run with --public to exercise the real Nix-backed make test-matrix entry.
"""

from contextlib import redirect_stdout
import io
import json
import os
from pathlib import Path
import re
import runpy
import shlex
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

SOURCE = Path(__file__).resolve().parent.parent
RUNNER = runpy.run_path(str(SOURCE / "admin/test-matrix"))
PUBLIC = "--public" in sys.argv
if PUBLIC:
    sys.argv.remove("--public")


class MatrixTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="forgejo-matrix-selftest-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)

    def test_exact_minimum(self):
        self.assertEqual(RUNNER["minimum_version"](SOURCE), "29.1")

    def test_snapshot_closed_manifest_and_symlinks(self):
        source = self.root / "input"
        (source / "admin").mkdir(parents=True)
        (source / "tests").mkdir()
        (source / "admin/sources").write_text("source.el\n")
        (source / "Makefile").touch()
        (source / "source.el").write_text("source")
        for name in ("private.el", "local.mk", "source.elc", "module.so"):
            (source / name).write_text("must not copy")
        destination = self.root / "output"
        RUNNER["snapshot"](source, destination)
        self.assertEqual(sorted(p.name for p in destination.iterdir()), ["Makefile", "source.el"])
        (source / "source.el").unlink()
        (source / "source.el").symlink_to(source / "private.el")
        with self.assertRaisesRegex(ValueError, "symlinked"):
            RUNNER["snapshot"](source, self.root / "rejected")
        (source / "tests/omitted.el").touch()
        with self.assertRaisesRegex(ValueError, "missing from admin/sources"):
            RUNNER["snapshot"](source, self.root / "omitted")

    def run_matrix(self, missing_fork=False, fail_minimum=False, missing_receipt=False):
        calls, resolved = [], []

        def resolve(command):
            resolved.append(command)
            if command == "user-fork" and missing_fork:
                raise ValueError("missing user-fork")
            return "/resolved/" + command

        def run(command, **_):
            name = command[command.index("--lane") + 1]
            calls.append(name)
            self.assertEqual(resolved[0], "user-fork")
            self.assertEqual(command[2].split("#")[0], str(SOURCE))
            if name == "fork":
                self.assertEqual(command[-2:], ["--emacs", "/resolved/user-fork"])
            if not missing_receipt:
                lane = Path(command[command.index("--root") + 1])
                lane.mkdir()
                (lane / "passed").write_text("29.1\n")
            return subprocess.CompletedProcess(command, 1 if fail_minimum and name == "minimum" else 0)

        with patch.dict(os.environ, {"THANOS_EMACS": "user-fork"}, clear=True), \
             patch.dict(RUNNER["matrix"].__globals__, {"executable": resolve}), \
             patch("tempfile.mkdtemp", return_value=str(self.root)), \
             patch("subprocess.check_output", return_value=json.dumps({"path": str(SOURCE)})), \
             patch("subprocess.run", side_effect=run), redirect_stdout(io.StringIO()):
            if missing_fork or fail_minimum or missing_receipt:
                with self.assertRaisesRegex(RuntimeError, "Required matrix lanes failed"):
                    RUNNER["matrix"](SOURCE)
            else:
                RUNNER["matrix"](SOURCE)
        return calls

    def test_resolve_before_nix(self):
        self.assertEqual(self.run_matrix(), ["minimum", "default", "fork"])

    def test_attempt_all(self):
        self.assertEqual(self.run_matrix(fail_minimum=True), ["minimum", "default", "fork"])

    def test_missing_fork(self):
        self.assertEqual(self.run_matrix(missing_fork=True), ["minimum", "default"])

    def test_missing_passed(self):
        self.assertEqual(self.run_matrix(missing_receipt=True), ["minimum", "default", "fork"])

    def test_invalid_statistics(self):
        for stats in ({}, {"total": 0, "completed": 0, "expected": 0, "unexpected": 0, "skipped": 0},
                      {"total": 2, "completed": 1, "expected": 1, "unexpected": 0, "skipped": 0},
                      {"total": 1, "completed": 1, "expected": 0, "unexpected": 1, "skipped": 0}):
            with self.subTest(stats=stats), self.assertRaises(ValueError):
                RUNNER["validate_stats"](stats)

    def test_wrong_patch_release(self):
        deps = self.root / "deps"
        deps.mkdir()
        with patch.dict(os.environ, {"FORGEJO_MATRIX_DEPS": str(deps)}), \
             patch("subprocess.check_output", return_value="29.4"), \
             patch("subprocess.run") as run, redirect_stdout(io.StringIO()):
            with self.assertRaisesRegex(ValueError, "expected 29.1, got 29.4"):
                RUNNER["lane"](SOURCE, self.root / "minimum", "minimum", "/fake/emacs", "29.1")
            run.assert_not_called()

    def writable(self, source):
        """Make owned fixture copies writable, including Nix store copies."""
        for path in [source, *source.rglob("*")]:
            path.chmod(path.stat().st_mode | 0o200)

    def fixture(self):
        source = self.root / "source"
        shutil.copytree(SOURCE / "admin", source / "admin")
        shutil.copy2(SOURCE / "Makefile", source / "Makefile")
        (source / "tests").mkdir()
        for name, assertion in (("good", "(should t)"), ("bad", "(should nil)")):
            (source / f"tests/forgejo-test-{name}.el").write_text(
                f"(require 'ert)\n(ert-deftest {name} () {assertion})\n"
                f"(provide 'forgejo-test-{name})\n")
        env = os.environ.copy()
        for key in ("MAKEFLAGS", "MFLAGS", "MAKEOVERRIDES", "MAKEFILES", "GNUMAKEFLAGS",
                    "FORGEJO_FULL_SUITE", "EMACSLOADPATH"):
            env.pop(key, None)
        for key in ("HOME", "TMPDIR", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME", "XDG_STATE_HOME"):
            env[key] = str(self.root / key)
            Path(env[key]).mkdir()
        env["FORGEJO_TEST_RESULTS"] = str(self.root / "results")
        env["FORGEJO_ENV_WRAPPED"] = "1"
        self.writable(source)
        return source, env

    def test_real_ert_positive_failure_and_early_exit(self):
        source, env = self.fixture()
        command = ["make", "do-test", "EMACS_CMD=" + RUNNER["executable"]("emacs")]
        good = subprocess.run(command + ["TESTS=tests/forgejo-test-good.el"], cwd=source,
                              env=env, text=True, capture_output=True)
        self.assertEqual(good.returncode, 0, good.stdout + good.stderr)
        bad = subprocess.run(command + ["TESTS=tests/forgejo-test-bad.el tests/forgejo-test-good.el"],
                             cwd=source, env=env, text=True, capture_output=True)
        self.assertNotEqual(bad.returncode, 0)
        self.assertIn("PASS tests/forgejo-test-good.el", bad.stdout)
        self.assertIn("unexpected", bad.stdout)
        (source / "tests/forgejo-test-good.el").write_text("(kill-emacs 0)\n")
        early = subprocess.run(command + ["TESTS=tests/forgejo-test-good.el"], cwd=source,
                               env=env, text=True, capture_output=True)
        self.assertNotEqual(early.returncode, 0)
        self.assertFalse((self.root / "results/summary.json").exists())
        self.assertFalse((self.root / "results/forgejo-test-good.json").exists())

    def test_public_selector_across_genuine_suites(self):
        source, env = self.fixture()
        shutil.copytree(SOURCE / "lisp", source / "lisp")
        shutil.copytree(SOURCE / "tests", source / "tests", dirs_exist_ok=True)
        self.writable(source)
        if os.environ.get("FORGEJO_MATRIX_DEPS"):
            env["EMACSLOADPATH"] = os.environ["FORGEJO_MATRIX_DEPS"] + os.pathsep
        suites = ["tests/forgejo-test-db.el", "tests/forgejo-test-filter.el"]
        command = ["make", "test", "EMACS_CMD=" + RUNNER["executable"]("emacs"),
                   "TESTS=" + " ".join(suites)]
        results = self.root / "results"
        for selector, expected in (('"forgejo-test-db-json-roundtrip"', 0),
                                   ('"no-such-forgejo-test"', 1)):
            result = subprocess.run(command + ["SELECTOR=" + selector], cwd=source,
                                    env=env, text=True, capture_output=True)
            if not expected:
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertEqual(json.loads((results / "summary.json").read_text()), suites)
                receipts = [json.loads((results / (Path(s).stem + ".json")).read_text()) for s in suites]
                self.assertEqual([r["total"] for r in receipts], [1, 0])
                self.assertEqual([n for r in receipts for n in r["names"]],
                                 ["forgejo-test-db-json-roundtrip"])
            else:
                self.assertNotEqual(result.returncode, 0)
                self.assertFalse((results / "summary.json").exists())
        # A zero-exit second suite is not a genuine zero-match receipt.
        (source / suites[1]).write_text("(kill-emacs 0)\n")
        (source / suites[1]).with_suffix(".elc").unlink(missing_ok=True)
        result = subprocess.run(command + ['SELECTOR="forgejo-test-db-json-roundtrip"'],
                                cwd=source, env=env, text=True, capture_output=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse((results / "summary.json").exists())
        self.assertFalse((results / "forgejo-test-filter.json").exists())

    def test_ordinary_inventory_and_new_early_exit_suite(self):
        source, env = self.fixture()
        (source / "tests/forgejo-test-bad.el").unlink()
        with (source / "Makefile").open("a") as output:
            output.write("\nSRCS =\nTEST_HELPERS = admin/forgejo-dev.el\n"
                         "TESTS = tests/forgejo-test-good.el\n")
        (source / "lisp/nested").mkdir(parents=True)
        RUNNER["ordinary_inputs"](source, env)
        nested = source / "lisp/nested/omitted.el"
        nested.write_text("(malformed\n")
        with (source / "admin/sources").open("a") as output:
            output.write("lisp/nested/omitted.el\n")
        with self.assertRaisesRegex(ValueError, "SRCS"):
            RUNNER["ordinary_inputs"](source, env)
        nested.unlink()
        added = "tests/forgejo-test-added.el"
        (source / added).write_text("(kill-emacs 0)\n")
        with (source / "admin/sources").open("a") as output:
            output.write(added + "\n")
        with self.assertRaisesRegex(ValueError, "TESTS"):
            RUNNER["ordinary_inputs"](source, env)
        with (source / "Makefile").open("a") as output:
            output.write("TESTS += " + added + "\n")
        RUNNER["ordinary_inputs"](source, env)
        env["FORGEJO_FULL_SUITE"] = "1"
        result = subprocess.run(["make", "test", "EMACS_CMD=" + RUNNER["executable"]("emacs")],
                                cwd=source, env=env, text=True, capture_output=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("PASS tests/forgejo-test-good.el", result.stdout)
        self.assertIn("FAIL tests/forgejo-test-added.el", result.stdout)
        self.assertFalse((self.root / "results/summary.json").exists())

    def test_recursive_snapshot_inventory(self):
        for directory in ("lisp", "tests"):
            with self.subTest(directory=directory):
                source = self.root / directory
                RUNNER["snapshot"](SOURCE, source)
                missing = source / directory / "nested/omitted.el"
                missing.parent.mkdir(parents=True)
                missing.touch()
                with self.assertRaisesRegex(ValueError, "missing from admin/sources"):
                    RUNNER["snapshot"](source, self.root / (directory + "-output"))

    def test_lane_make_startup_controls(self):
        deps = self.root / "deps"
        deps.mkdir()
        real_output = subprocess.check_output
        observed = []

        def inspect(command, **kwargs):
            observed.append(command)
            for key in ("MAKEFILES", "GNUMAKEFLAGS", "MAKEFLAGS", "MFLAGS", "MAKEOVERRIDES"):
                self.assertNotIn(key, kwargs["env"])
            return real_output(command, **kwargs)

        with patch.dict(os.environ, {"FORGEJO_MATRIX_DEPS": str(deps),
                                     "MAKEFILES": "unwanted.mk", "GNUMAKEFLAGS": "--eval=bad",
                                     "MAKEFLAGS": "-n", "MFLAGS": "-n", "MAKEOVERRIDES": "bad"}), \
             patch("subprocess.check_output", side_effect=inspect), redirect_stdout(io.StringIO()):
            with self.assertRaisesRegex(ValueError, "expected impossible"):
                RUNNER["lane"](SOURCE, self.root / "lane", "minimum",
                               RUNNER["executable"]("emacs"), "impossible")
        self.assertTrue(observed)

    def test_missing_summary_and_duplicate_identities(self):
        with self.assertRaises(FileNotFoundError):
            RUNNER["verify_suites"](self.root, ["tests/one.el"])
        (self.root / "summary.json").write_text('["tests/one.el"]')
        (self.root / "one.json").write_text(json.dumps({"suite": "tests/one.el", "total": 2,
            "completed": 2, "expected": 2, "unexpected": 0, "skipped": 0,
            "names": ["same", "same"], "sqlite": True, "libxml": True}))
        with self.assertRaisesRegex(ValueError, "identities"):
            RUNNER["verify_suites"](self.root, ["tests/one.el"])

    @unittest.skipUnless(PUBLIC, "explicit --public runs real Nix-backed matrix")
    def test_public_makefiles_cannot_replace_runtime(self):
        other = subprocess.check_output(
            ["nix", "develop", str(SOURCE) + "#matrix-minimum", "--command", "sh", "-c",
             'command -v emacs'], text=True).strip()
        marker = self.root / "injected-runtime-calls"
        wrapper = self.root / "other-emacs"
        wrapper.write_text("#!" + RUNNER["executable"]("sh") + "\n"
                           "printf '%s\\n' \"$*\" >> " + shlex.quote(str(marker)) + "\n"
                           "exec " + shlex.quote(other) + " \"$@\"\n")
        wrapper.chmod(0o700)
        startup = self.root / "injected.mk"
        startup.write_text("override EMACS_CMD := " + str(wrapper) + "\n")
        env = os.environ.copy()
        env.update(MAKEFILES=str(startup), MATRIX_TESTS="tests/forgejo-test-db.el")
        result = subprocess.run(["make", "test-matrix"], cwd=SOURCE, env=env,
                                text=True, capture_output=True)
        print(result.stdout, result.stderr)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertFalse(marker.exists(), marker.read_text() if marker.exists() else "")

    @unittest.skipUnless(PUBLIC, "explicit --public runs real Nix-backed matrix")
    def test_public_version_only_false_executable(self):
        real = RUNNER["executable"](os.environ.get("THANOS_EMACS", "emacs"))
        fake = self.root / "false-emacs"
        fake.write_text("#!" + RUNNER["executable"]("sh") + "\n"
                        "case \"$*\" in *'(princ emacs-version)'*) exec " + shlex.quote(real) + " \"$@\";; esac\nexit 0\n")
        fake.chmod(0o700)
        env = os.environ.copy()
        env["MATRIX_TESTS"] = "tests/forgejo-test-db.el"
        result = subprocess.run(["make", "test-matrix", "THANOS_EMACS=" + str(fake)],
                                cwd=SOURCE, env=env, text=True, capture_output=True)
        print(result.stdout, result.stderr)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("PASS Package-Requires minimum", result.stdout)
        self.assertIn("PASS Pinned nixpkgs default Emacs", result.stdout)
        self.assertIn("FAIL Thanos Emacs fork", result.stdout)
        self.assertNotIn("PASS all three", result.stdout)
        match = re.search(r"Matrix evidence: (.+)", result.stdout)
        self.assertIsNotNone(match)
        assert match is not None
        evidence = Path(match.group(1))
        self.assertFalse((evidence / "fork/passed").exists())
        self.assertFalse(list((evidence / "fork/ert").glob("*.json")))


if __name__ == "__main__":
    unittest.main()
