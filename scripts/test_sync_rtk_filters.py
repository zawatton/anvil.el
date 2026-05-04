from __future__ import annotations

import subprocess
import tempfile
import textwrap
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent.parent
SCRIPT = REPO_ROOT / "scripts" / "sync-rtk-filters.py"


class SyncRtkFiltersTest(unittest.TestCase):
    def run_sync(self, vendor_dir: Path, output_path: Path, *extra_args: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["python3", str(SCRIPT), "--vendor-dir", str(vendor_dir), "--output", str(output_path), *extra_args],
            capture_output=True,
            text=True,
            cwd=REPO_ROOT,
            check=False,
        )

    def write_toml(self, vendor_dir: Path, name: str, content: str) -> None:
        (vendor_dir / name).write_text(textwrap.dedent(content), encoding="utf-8")

    def test_minimal_filter(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            vendor_dir = Path(tmp) / "vendor"
            output_path = Path(tmp) / "out.el"
            vendor_dir.mkdir()
            self.write_toml(
                vendor_dir,
                "foo.toml",
                """
                [filters.foo]
                match_command = "cmd"
                max_lines = 5
                """,
            )

            result = self.run_sync(vendor_dir, output_path)

            self.assertEqual(result.returncode, 0, result.stderr)
            output = output_path.read_text(encoding="utf-8")
            self.assertIn(
                "(anvil-shell-filter-register-from-spec '(:tag foo :match-command \"cmd\" :max-lines 5))",
                output,
            )

    def test_snake_to_kebab(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            vendor_dir = Path(tmp) / "vendor"
            output_path = Path(tmp) / "out.el"
            vendor_dir.mkdir()
            self.write_toml(
                vendor_dir,
                "foo_bar.toml",
                """
                [filters.foo_bar]
                description = "desc"
                match_command = "cmd"
                strip_ansi = true
                filter_stderr = true
                strip_lines_matching = ["skip"]
                keep_lines_matching = ["keep"]
                truncate_lines_at = 7
                max_lines = 5
                tail_lines = 2
                on_empty = "empty"
                """,
            )

            result = self.run_sync(vendor_dir, output_path)

            self.assertEqual(result.returncode, 0, result.stderr)
            output = output_path.read_text(encoding="utf-8")
            self.assertIn(":tag foo-bar", output)
            self.assertIn(":match-command \"cmd\"", output)
            self.assertIn(":strip-ansi t", output)
            self.assertIn(":filter-stderr t", output)
            self.assertIn(":strip-lines-matching (\"skip\")", output)
            self.assertIn(":keep-lines-matching (\"keep\")", output)
            self.assertIn(":truncate-lines-at 7", output)
            self.assertIn(":max-lines 5", output)
            self.assertIn(":tail-lines 2", output)
            self.assertIn(":on-empty \"empty\"", output)

    def test_bool_true_false(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            vendor_dir = Path(tmp) / "vendor"
            output_path = Path(tmp) / "out.el"
            vendor_dir.mkdir()
            self.write_toml(
                vendor_dir,
                "bool.toml",
                """
                [filters.foo]
                match_command = "cmd"
                strip_ansi = true

                [filters.bar]
                match_command = "cmd2"
                """,
            )

            result = self.run_sync(vendor_dir, output_path)

            self.assertEqual(result.returncode, 0, result.stderr)
            output = output_path.read_text(encoding="utf-8")
            self.assertIn(":strip-ansi t", output)
            bar_line = next(
                line for line in output.splitlines() if "(:tag bar " in line
            )
            self.assertNotIn(":strip-ansi", bar_line)

    def test_strip_lines_array(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            vendor_dir = Path(tmp) / "vendor"
            output_path = Path(tmp) / "out.el"
            vendor_dir.mkdir()
            self.write_toml(
                vendor_dir,
                "strip.toml",
                """
                [filters.foo]
                match_command = "cmd"
                strip_lines_matching = ["alpha", "beta"]
                """,
            )

            result = self.run_sync(vendor_dir, output_path)

            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn(
                ":strip-lines-matching (\"alpha\" \"beta\")",
                output_path.read_text(encoding="utf-8"),
            )

    def test_replace_table_array(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            vendor_dir = Path(tmp) / "vendor"
            output_path = Path(tmp) / "out.el"
            vendor_dir.mkdir()
            self.write_toml(
                vendor_dir,
                "replace.toml",
                """
                [filters.foo]
                match_command = "cmd"

                [[filters.foo.replace]]
                pattern = "X"
                replacement = "Y"
                """,
            )

            result = self.run_sync(vendor_dir, output_path)

            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn(
                ":replace ((\"X\" . \"Y\"))",
                output_path.read_text(encoding="utf-8"),
            )

    def test_match_output_table_array(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            vendor_dir = Path(tmp) / "vendor"
            output_path = Path(tmp) / "out.el"
            vendor_dir.mkdir()
            self.write_toml(
                vendor_dir,
                "match-output.toml",
                """
                [filters.foo]
                match_command = "cmd"

                [[filters.foo.match_output]]
                pattern = "WARN"
                message = "Heads up"
                """,
            )

            result = self.run_sync(vendor_dir, output_path)

            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn(
                ":match-output ((\"WARN\" . \"Heads up\"))",
                output_path.read_text(encoding="utf-8"),
            )

    def test_empty_vendor_dir_exit_zero(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            vendor_dir = Path(tmp) / "vendor"
            output_path = Path(tmp) / "out.el"
            vendor_dir.mkdir()

            result = self.run_sync(vendor_dir, output_path)

            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertFalse(output_path.exists())
            self.assertIn("No TOML filter specs found", result.stderr)

    def test_alphabetical_ordering(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            vendor_dir = Path(tmp) / "vendor"
            output_path = Path(tmp) / "out.el"
            vendor_dir.mkdir()
            self.write_toml(
                vendor_dir,
                "zeta.toml",
                """
                [filters.zeta]
                match_command = "z"
                """,
            )
            self.write_toml(
                vendor_dir,
                "alpha.toml",
                """
                [filters.alpha]
                match_command = "a"
                """,
            )

            result = self.run_sync(vendor_dir, output_path)

            self.assertEqual(result.returncode, 0, result.stderr)
            output = output_path.read_text(encoding="utf-8")
            self.assertLess(output.index("(:tag alpha "), output.index("(:tag zeta "))

    def test_check_mode_exit_one_on_drift(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            vendor_dir = Path(tmp) / "vendor"
            output_path = Path(tmp) / "out.el"
            vendor_dir.mkdir()
            self.write_toml(
                vendor_dir,
                "foo.toml",
                """
                [filters.foo]
                match_command = "cmd"
                """,
            )

            first = self.run_sync(vendor_dir, output_path)
            self.assertEqual(first.returncode, 0, first.stderr)
            output_path.write_text("drift\n", encoding="utf-8")

            result = self.run_sync(vendor_dir, output_path, "--check")

            self.assertEqual(result.returncode, 1)
            self.assertIn("out of date", result.stderr)

    def test_string_quoting(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            vendor_dir = Path(tmp) / "vendor"
            output_path = Path(tmp) / "out.el"
            vendor_dir.mkdir()
            self.write_toml(
                vendor_dir,
                "quote.toml",
                r'''
                [filters.foo]
                match_command = "line1\nline2"
                description = "say \"hi\" \\ path"
                ''',
            )

            result = self.run_sync(vendor_dir, output_path)

            self.assertEqual(result.returncode, 0, result.stderr)
            output = output_path.read_text(encoding="utf-8")
            self.assertIn(':match-command "line1\\nline2"', output)
            self.assertIn(':description "say \\"hi\\" \\\\ path"', output)


if __name__ == "__main__":
    unittest.main()
