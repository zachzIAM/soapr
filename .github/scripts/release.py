#!/usr/bin/env python3
"""Zero-dependency SemVer release calculator and changelog updater for soapr."""

from __future__ import annotations

import argparse
from dataclasses import dataclass
import datetime as dt
import os
from pathlib import Path
import re
import subprocess
import sys

CONVENTIONAL_HEADER_RE = re.compile(
    r"^(?P<type>[A-Za-z]+)(?:\((?P<scope>[^)]+)\))?(?P<breaking>!)?:\s*(?P<desc>.+)$"
)
BREAKING_FOOTER_RE = re.compile(
    r"^(?:BREAKING CHANGE|BREAKING-CHANGE):\s*(?P<note>.*)$",
    re.MULTILINE,
)

BUMP_NONE = 0
BUMP_PATCH = 1
BUMP_MINOR = 2
BUMP_MAJOR = 3

PATCH_TYPES = frozenset({"fix", "perf"})
MINOR_TYPES = frozenset({"feat"})


@dataclass(frozen=True)
class ParsedCommit:
    """Parsed Conventional Commit metadata."""

    sha: str
    subject: str
    commit_type: str | None
    scope: str | None
    description: str
    breaking: bool
    breaking_note: str | None
    bump: int


def parse_commit(sha: str, subject: str, body: str = "") -> ParsedCommit:
    """Parse a commit header and body according to Conventional Commits."""
    subject_clean = subject.strip()
    body_clean = body or ""
    header_match = CONVENTIONAL_HEADER_RE.match(subject_clean)
    footer_match = BREAKING_FOOTER_RE.search(body_clean)

    breaking_note: str | None = None
    if footer_match:
        note = footer_match.group("note").strip()
        breaking_note = note if note else None

    if not header_match:
        is_breaking = footer_match is not None
        return ParsedCommit(
            sha=sha,
            subject=subject_clean,
            commit_type=None,
            scope=None,
            description=subject_clean,
            breaking=is_breaking,
            breaking_note=breaking_note,
            bump=BUMP_MAJOR if is_breaking else BUMP_NONE,
        )

    commit_type = header_match.group("type").lower()
    scope = header_match.group("scope")
    description = header_match.group("desc").strip()
    is_breaking = bool(header_match.group("breaking")) or (footer_match is not None)

    if is_breaking:
        bump = BUMP_MAJOR
    elif commit_type in MINOR_TYPES:
        bump = BUMP_MINOR
    elif commit_type in PATCH_TYPES:
        bump = BUMP_PATCH
    else:
        bump = BUMP_NONE

    return ParsedCommit(
        sha=sha,
        subject=subject_clean,
        commit_type=commit_type,
        scope=scope,
        description=description,
        breaking=is_breaking,
        breaking_note=breaking_note,
        bump=bump,
    )


def determine_bump(commits: list[ParsedCommit]) -> int:
    """Return the highest SemVer bump level across commits."""
    if not commits:
        return BUMP_NONE
    return max(commit.bump for commit in commits)


def parse_description(description_path: Path) -> tuple[str, str]:
    """Read Package and Version from a DESCRIPTION file."""
    text = description_path.read_text(encoding="utf-8")
    pkg_match = re.search(r"^Package:\s*(.+)$", text, re.MULTILINE)
    ver_match = re.search(r"^Version:\s*(.+)$", text, re.MULTILINE)
    if not ver_match:
        raise ValueError(f"Could not find Version field in {description_path}")
    package_name = pkg_match.group(1).strip() if pkg_match else "soapr"
    version = ver_match.group(1).strip()
    return package_name, version


def bump_version(current_version: str, bump: int) -> str:
    """Apply strict SemVer bump, stripping any trailing .9000 dev suffix."""
    if bump == BUMP_NONE:
        raise ValueError("Cannot bump version when bump level is BUMP_NONE")

    match = re.match(r"^(\d+)\.(\d+)(?:\.(\d+))?(?:\.\d+)?$", current_version.strip())
    if not match:
        raise ValueError(f"Unsupported version format: {current_version!r}")

    major = int(match.group(1))
    minor = int(match.group(2))
    patch = int(match.group(3)) if match.group(3) is not None else 0

    if bump == BUMP_MAJOR:
        return f"{major + 1}.0.0"
    if bump == BUMP_MINOR:
        return f"{major}.{minor + 1}.0"
    return f"{major}.{minor}.{patch + 1}"


def update_description_version(description_path: Path, new_version: str) -> None:
    """Update only the Version line in DESCRIPTION, preserving all other content."""
    text = description_path.read_text(encoding="utf-8")
    updated, count = re.subn(
        r"^(Version:\s*)(.+)$",
        rf"\g<1>{new_version}",
        text,
        count=1,
        flags=re.MULTILINE,
    )
    if count != 1:
        raise ValueError(f"Failed to update Version field in {description_path}")
    description_path.write_text(updated, encoding="utf-8")


def _format_entry(commit: ParsedCommit, text: str) -> str:
    short_sha = commit.sha[:7]
    prefix = f"**{commit.scope}:** " if commit.scope else ""
    return f"- {prefix}{text} ({short_sha})"


def format_release_notes(
    package_name: str,
    new_version: str,
    commits: list[ParsedCommit],
    release_date: str | None = None,
) -> str:
    """Render a NEWS.md section grouped into Breaking Changes, Features, and Bug Fixes."""
    date_str = release_date or dt.date.today().isoformat()
    breaking_items: list[str] = []
    feature_items: list[str] = []
    fix_items: list[str] = []

    for commit in commits:
        if commit.breaking:
            note = commit.breaking_note or commit.description
            breaking_items.append(_format_entry(commit, note))
        elif commit.commit_type in MINOR_TYPES:
            feature_items.append(_format_entry(commit, commit.description))
        elif commit.commit_type in PATCH_TYPES:
            fix_items.append(_format_entry(commit, commit.description))

    lines = [f"# {package_name} {new_version} ({date_str})"]
    for heading, items in (
        ("Breaking Changes", breaking_items),
        ("Features", feature_items),
        ("Bug Fixes", fix_items),
    ):
        if items:
            lines.extend(["", f"### {heading}", *items])

    return "\n".join(lines) + "\n"


def update_news(news_path: Path, release_section: str) -> None:
    """Create NEWS.md or prepend the new release section above existing notes."""
    section_clean = release_section.strip()
    if not news_path.exists():
        news_path.write_text(section_clean + "\n", encoding="utf-8")
        return

    existing = news_path.read_text(encoding="utf-8").lstrip("\n")
    if existing:
        news_path.write_text(f"{section_clean}\n\n{existing}", encoding="utf-8")
    else:
        news_path.write_text(section_clean + "\n", encoding="utf-8")


def _run_git(repo_path: Path, args: list[str], check: bool = True) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["git", *args],
        cwd=repo_path,
        check=check,
        capture_output=True,
        text=True,
    )


def get_commit_range(repo_path: Path) -> str:
    """Determine git log range since the latest v* tag, or HEAD~1..HEAD on first release."""
    tag_proc = _run_git(
        repo_path,
        ["describe", "--tags", "--abbrev=0", "--match", "v*"],
        check=False,
    )
    if tag_proc.returncode == 0:
        latest_tag = tag_proc.stdout.strip()
        if latest_tag:
            return f"{latest_tag}..HEAD"

    parent_proc = _run_git(repo_path, ["rev-parse", "--verify", "HEAD~1"], check=False)
    if parent_proc.returncode == 0:
        return "HEAD~1..HEAD"

    return "HEAD"


def get_commits_in_range(repo_path: Path, rev_range: str) -> list[ParsedCommit]:
    """Read and parse non-merge commits in rev_range."""
    log_proc = _run_git(
        repo_path,
        ["log", "--no-merges", "--format=%H%x1f%s%x1f%b%x1e", rev_range],
    )
    raw = log_proc.stdout
    commits: list[ParsedCommit] = []
    for record in raw.split("\x1e"):
        if not record.strip():
            continue
        parts = record.split("\x1f", 2)
        if len(parts) < 2:
            continue
        sha = parts[0].strip()
        subject = parts[1].strip()
        body = parts[2].strip() if len(parts) > 2 else ""
        commits.append(parse_commit(sha, subject, body))
    return commits


def run_release(
    repo_path: Path,
    notes_output: Path | None = None,
    release_date: str | None = None,
) -> dict[str, str]:
    """Evaluate commits, bump DESCRIPTION and NEWS.md when warranted, and return outputs."""
    rev_range = get_commit_range(repo_path)
    commits = get_commits_in_range(repo_path, rev_range)
    bump = determine_bump(commits)

    if bump == BUMP_NONE:
        return {"released": "false", "version": "", "tag": ""}

    description_path = repo_path / "DESCRIPTION"
    news_path = repo_path / "NEWS.md"

    package_name, current_version = parse_description(description_path)
    new_version = bump_version(current_version, bump)
    tag = f"v{new_version}"
    notes = format_release_notes(package_name, new_version, commits, release_date=release_date)

    update_description_version(description_path, new_version)
    update_news(news_path, notes)

    if notes_output is not None:
        notes_output.parent.mkdir(parents=True, exist_ok=True)
        notes_output.write_text(notes, encoding="utf-8")

    return {"released": "true", "version": new_version, "tag": tag}


def write_github_outputs(outputs: dict[str, str], output_path: Path) -> None:
    """Append key=value lines to $GITHUB_OUTPUT."""
    with output_path.open("a", encoding="utf-8") as fh:
        for key, value in outputs.items():
            fh.write(f"{key}={value}\n")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Calculate SemVer bump and update DESCRIPTION/NEWS.md.")
    parser.add_argument("--repo", type=Path, default=Path("."), help="Path to git repository root.")
    parser.add_argument("--notes-output", type=Path, default=None, help="Path to write release notes.")
    parser.add_argument("--github-output", type=Path, default=None, help="Path to GITHUB_OUTPUT file.")
    parser.add_argument("--date", type=str, default=None, help="Override release date (YYYY-MM-DD).")
    args = parser.parse_args(argv)

    outputs = run_release(
        repo_path=args.repo.resolve(),
        notes_output=args.notes_output,
        release_date=args.date,
    )

    gh_out = args.github_output or (Path(os.environ["GITHUB_OUTPUT"]) if os.environ.get("GITHUB_OUTPUT") else None)
    if gh_out is not None:
        write_github_outputs(outputs, gh_out)

    print(
        f"released={outputs['released']} version={outputs['version']} tag={outputs['tag']}",
        file=sys.stdout,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
