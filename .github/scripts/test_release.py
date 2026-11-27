#!/usr/bin/env python3
"""Unit and integration tests for .github/scripts/release.py."""

from __future__ import annotations

from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parent))
import release  # noqa: E402


SAMPLE_DESCRIPTION = """\
Type: Package
Package: soapr
Title: SOAP API Client Generator
Version: 0.1.0
Authors@R: person("Test", "Author", email = "test@example.com", role = c("aut", "cre"))
Description: A robust SOAP client generator for R.
License: MIT + file LICENSE
Encoding: UTF-8
"""


def _git(repo: Path, *args: str) -> str:
    proc = subprocess.run(
        ["git", *args],
        cwd=repo,
        check=True,
        capture_output=True,
        text=True,
    )
    return proc.stdout.strip()


def _init_repo(repo: Path, description: str = SAMPLE_DESCRIPTION) -> None:
    _git(repo, "init")
    _git(repo, "config", "user.name", "Test User")
    _git(repo, "config", "user.email", "test@example.com")
    (repo / "DESCRIPTION").write_text(description, encoding="utf-8")
    _git(repo, "add", "DESCRIPTION")
    _git(repo, "commit", "-m", "initial commit before conventional history")


def _commit_file(repo: Path, filename: str, content: str, message: str) -> str:
    (repo / filename).write_text(content, encoding="utf-8")
    _git(repo, "add", filename)
    _git(repo, "commit", "-m", message)
    return _git(repo, "rev-parse", "HEAD")


class ReleaseUnitTests(unittest.TestCase):
    def test_breaking_bang_triggers_major(self) -> None:
        c = release.parse_commit("abc1234", "feat!: drop SOAP 1.0 support")
        self.assertEqual(c.bump, release.BUMP_MAJOR)
        self.assertTrue(c.breaking)
        self.assertEqual(release.bump_version("0.1.0", c.bump), "1.0.0")

    def test_scoped_breaking_bang_triggers_major(self) -> None:
        c = release.parse_commit("abc1234", "fix(client)!: change transport signature")
        self.assertEqual(c.bump, release.BUMP_MAJOR)
        self.assertEqual(c.scope, "client")
        self.assertEqual(release.bump_version("0.4.2", c.bump), "1.0.0")

    def test_breaking_footer_triggers_major(self) -> None:
        c = release.parse_commit(
            "abc1234",
            "refactor(factory): simplify return structure",
            body="Some details.\n\nBREAKING CHANGE: returns named list instead of vector",
        )
        self.assertEqual(c.bump, release.BUMP_MAJOR)
        self.assertTrue(c.breaking)
        self.assertEqual(c.breaking_note, "returns named list instead of vector")

        c_hyphen = release.parse_commit(
            "def5678",
            "feat: update parser",
            body="BREAKING-CHANGE: strict schema validation",
        )
        self.assertEqual(c_hyphen.bump, release.BUMP_MAJOR)
        self.assertEqual(c_hyphen.breaking_note, "strict schema validation")

    def test_feat_and_scoped_feat_trigger_minor(self) -> None:
        c1 = release.parse_commit("1111111", "feat: add MTOM attachment support")
        c2 = release.parse_commit("2222222", "feat(wsdl): support xsd:import")
        self.assertEqual(c1.bump, release.BUMP_MINOR)
        self.assertEqual(c2.bump, release.BUMP_MINOR)
        self.assertEqual(c2.scope, "wsdl")
        self.assertEqual(release.bump_version("0.1.0", c1.bump), "0.2.0")

    def test_fix_and_perf_trigger_patch(self) -> None:
        c_fix = release.parse_commit("1111111", "fix: handle empty soap:Body")
        c_scoped_fix = release.parse_commit("2222222", "fix(xml): escape attribute quotes")
        c_perf = release.parse_commit("3333333", "perf(parser): cache XSD lookups")
        self.assertEqual(c_fix.bump, release.BUMP_PATCH)
        self.assertEqual(c_scoped_fix.bump, release.BUMP_PATCH)
        self.assertEqual(c_perf.bump, release.BUMP_PATCH)
        self.assertEqual(release.bump_version("0.1.0", c_fix.bump), "0.1.1")

    def test_bump_precedence_breaking_over_feat_over_fix(self) -> None:
        commits = [
            release.parse_commit("1111111", "fix: minor bug"),
            release.parse_commit("2222222", "feat: new feature"),
        ]
        self.assertEqual(release.determine_bump(commits), release.BUMP_MINOR)

        commits.append(release.parse_commit("3333333", "fix!: breaking fix"))
        self.assertEqual(release.determine_bump(commits), release.BUMP_MAJOR)

    def test_non_releasing_commits_produce_no_release(self) -> None:
        non_releasing = [
            "chore: tidy up files",
            "ci: add lint workflow",
            "docs: update README",
            "test: add unit tests",
            "style: fix indentation",
            "refactor: extract helper",
            "Merge pull request #10 from org/branch",
        ]
        parsed = [release.parse_commit("1234567", msg) for msg in non_releasing]
        self.assertEqual(release.determine_bump(parsed), release.BUMP_NONE)

    def test_dev_suffix_stripped_before_bump(self) -> None:
        self.assertEqual(release.bump_version("0.1.0.9000", release.BUMP_PATCH), "0.1.1")
        self.assertEqual(release.bump_version("0.1.0.9000", release.BUMP_MINOR), "0.2.0")
        self.assertEqual(release.bump_version("0.1.0.9000", release.BUMP_MAJOR), "1.0.0")


class ReleaseIntegrationTests(unittest.TestCase):
    def test_first_release_ignores_pre_conventional_history_and_creates_news(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            repo = Path(tmp)
            _init_repo(repo)
            # Pre-conventional commit that would have bumped if full history were scanned
            _commit_file(repo, "a.txt", "1", "feat: old pre-release work")
            # Simulate PR merge commit bringing in a fix
            _git(repo, "checkout", "-b", "fix/bug")
            fix_sha = _commit_file(repo, "b.txt", "2", "fix(client): handle SOAP 1.2 faults")
            _git(repo, "checkout", "main") if "main" in _git(repo, "branch") else _git(repo, "checkout", "master")
            _git(repo, "merge", "--no-ff", "fix/bug", "-m", "Merge pull request #1 from fix/bug")

            notes_out = repo / "tmp_notes.md"
            result = release.run_release(repo, notes_output=notes_out, release_date="2026-11-27")

            self.assertEqual(result, {"released": "true", "version": "0.1.1", "tag": "v0.1.1"})
            desc = (repo / "DESCRIPTION").read_text(encoding="utf-8")
            self.assertIn("Version: 0.1.1\n", desc)
            self.assertIn("Title: SOAP API Client Generator", desc)

            news = (repo / "NEWS.md").read_text(encoding="utf-8")
            expected_section = (
                "# soapr 0.1.1 (2026-11-27)\n\n"
                "### Bug Fixes\n"
                f"- **client:** handle SOAP 1.2 faults ({fix_sha[:7]})\n"
            )
            self.assertEqual(news, expected_section)
            self.assertEqual(notes_out.read_text(encoding="utf-8"), expected_section)

    def test_subsequent_release_uses_latest_tag_and_prepends_news(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            repo = Path(tmp)
            _init_repo(repo)
            _commit_file(repo, "a.txt", "1", "fix: initial fix")
            (repo / "NEWS.md").write_text("# soapr 0.1.1 (2026-11-01)\n\n### Bug Fixes\n- old fix\n", encoding="utf-8")
            (repo / "DESCRIPTION").write_text(SAMPLE_DESCRIPTION.replace("0.1.0", "0.1.1"), encoding="utf-8")
            _git(repo, "add", "DESCRIPTION", "NEWS.md")
            _git(repo, "commit", "-m", "chore(release): v0.1.1")
            _git(repo, "tag", "-a", "v0.1.1", "-m", "Release v0.1.1")

            feat_sha = _commit_file(repo, "b.txt", "2", "feat: add async transport")
            fix_sha = _commit_file(repo, "c.txt", "3", "fix: trim whitespace")

            result = release.run_release(repo, release_date="2026-11-27")
            self.assertEqual(result, {"released": "true", "version": "0.2.0", "tag": "v0.2.0"})

            desc = (repo / "DESCRIPTION").read_text(encoding="utf-8")
            self.assertIn("Version: 0.2.0\n", desc)

            news = (repo / "NEWS.md").read_text(encoding="utf-8")
            self.assertTrue(news.startswith("# soapr 0.2.0 (2026-11-27)\n"))
            self.assertIn(f"- add async transport ({feat_sha[:7]})", news)
            self.assertIn(f"- trim whitespace ({fix_sha[:7]})", news)
            self.assertIn("# soapr 0.1.1 (2026-11-01)", news)
            self.assertLess(news.index("0.2.0"), news.index("0.1.1"))

    def test_non_releasing_commit_after_tag_makes_no_changes(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            repo = Path(tmp)
            _init_repo(repo)
            _git(repo, "tag", "-a", "v0.1.0", "-m", "Release v0.1.0")
            _commit_file(repo, "ci.yml", "name: ci", "ci: update workflow")
            _commit_file(repo, "README.md", "# docs", "docs: clarify install")

            result = release.run_release(repo, release_date="2026-11-27")
            self.assertEqual(result, {"released": "false", "version": "", "tag": ""})
            self.assertIn("Version: 0.1.0\n", (repo / "DESCRIPTION").read_text(encoding="utf-8"))
            self.assertFalse((repo / "NEWS.md").exists())


if __name__ == "__main__":
    unittest.main()
