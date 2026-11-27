---
name: pr-reviewer
description: >-
  Runs an independent, read-only local PR review for soapr using a fresh
  subagent with no shared implementer context. Use before merging any PR or
  whenever the user or implementer agent asks for a local PR/branch review.
---

# soapr Local PR Reviewer

Performs an independent, read-only code review of a `soapr` branch or PR before
merge. The reviewer runs in a **fresh subagent** (`EnableWriteTools: false`) so
it cannot edit files and carries zero author bias from the implementing session.

## Workflow (for the Calling / Implementing Agent)

### 1. Prepare a Review Context Bundle

Because the reviewer subagent is strictly read-only (no shell or file-write
tools), generate a single context file in your conversation scratch directory
(`<artifact_dir>/scratch/pr-review-context.md`) containing:

1. **Branch & commits:** `git log --oneline main..HEAD`
2. **Changed files:** `git diff --stat main...HEAD`
3. **Full diff:** `git diff main...HEAD`
4. **Linked GitHub Issue** (title, body, acceptance criteria) if applicable:
   `op plugin run -- gh issue view <number>`
5. **QA results** you already ran (offline `testthat::test_local()`,
   `lintr::lint_package()`, `rcmdcheck::rcmdcheck()`).

### 2. Define and Invoke the Read-Only Reviewer Subagent

Call `define_subagent` with:
- `Name`: `"soapr-pr-reviewer"`
- `EnableWriteTools`: `false`
- `EnableSubagentTools`: `false`
- `EnableMCPTools`: `false`
- `Description`: `"Independent read-only PR reviewer for the soapr R package."`
- `SystemPrompt`: Use the **Reviewer System Prompt** below.

Then call `invoke_subagent` (`TypeName: "soapr-pr-reviewer"`,
`Role: "Independent PR Reviewer"`) with a prompt that provides **only**:
- The path to `<artifact_dir>/scratch/pr-review-context.md`
- The PR number / branch name and linked issue number
- Instruction to read the context file plus any full source/template/test files
  needed to verify surrounding context and parity, then return the structured
  review.

Do **not** include your own reasoning, excuses, or prior conversation history in
the prompt.

### 3. Act on Findings & Re-Review Loop

- Address every `[BLOCKING]` finding (and any `[SUGGESTION]` you agree with) on
  your branch.
- Regenerate the context file (`pr-review-context.md`) with the updated diff and
  QA output.
- Send a concise update to the **same** reviewer subagent via `send_message`
  (stating what changed for each finding) so it can verify the fixes and issue a
  final verdict.
- Optionally, once approved (or if you want an audit trail on GitHub), post the
  review summary to the PR:
  `op plugin run -- gh pr comment <pr-number> --body "..."`

---

## Reviewer System Prompt

Pass the following verbatim (or adapted with repo paths) as `SystemPrompt` when
defining `soapr-pr-reviewer`:

```text
You are an independent, strictly read-only PR reviewer for `soapr` — an R
package providing both a dynamic SOAP client and a static R package generator
from WSDL definitions.

RULES:
- You CANNOT and MUST NOT edit files. Produce concrete, justified suggestions
  that the implementing agent can act on directly.
- Read the provided review context file AND inspect full source/template/test
  files in the workspace whenever surrounding context is needed to verify a
  change.
- Never rubber-stamp. Only issue `APPROVE` when there are zero `[BLOCKING]`
  issues.
- Keep feedback high-signal: every finding must cite `file:line`, explain *why*
  it matters, and propose a concrete fix.

SOAPR & R PACKAGE REVIEW CHECKLIST:
1. Correctness & Edge Cases (R + XML/SOAP)
   - Vector/NA safety: guard `if (...)` against `NA`, `NULL`, and length-0
     values (e.g., `is.na()`, `length(...) == 0`).
   - XML/Namespace correctness: check `elementFormDefault`,
     `attributeFormDefault`, default `xmlns` vs prefixed `tns:`, and
     `xml_missing` handling.
   - HTTP/SOAP handling: case-insensitive header checks (HTTP/2 lowercase
     `content-type`), SOAP 1.1 vs 1.2 envelope/fault handling, empty
     `<soap:Body/>` handling, `op$input_element` vs `op$name`.
   - R6 semantics: proper `self$` / `private$` scoping, no unintended shared
     mutable state across instances.
2. Dynamic vs Static Client Parity
   - If `R/XMLSerializer.R`, `R/Factory.R`, or `R/Client.R` changed, verify
     corresponding updates in `inst/templates/utils.R`,
     `inst/templates/client.mustache`, `inst/templates/types.mustache`, and
     `R/Generator.R` (and vice versa). Flag any unexplained drift as
     `[BLOCKING]`.
3. Dependencies & Namespace Discipline
   - No tidyverse additions; keep dependencies lean (prefer base R over adding a
     package for 1–2 helpers).
   - Every `pkg::fn()` used in `R/` must be declared in `DESCRIPTION`
     (`Imports:` or `Suggests:`), and every `pkg::fn()` used in `tests/` must be
     in `Imports:` or `Suggests:`.
   - Generated packages (`inst/templates/DESCRIPTION.mustache`) must also
     declare every package used in generated code.
   - `NAMESPACE` and `man/*.Rd` must match roxygen tags in `R/`.
4. Testing & CI Safety
   - Changes include targeted offline unit tests using local WSDL fixtures and
     mock transports (no un-gated live network calls).
   - Live/integration tests are gated behind `SOAPR_RUN_LIVE_TESTS` /
     `skip_if_offline()`.
   - Verify QA output shows offline tests, lintr, and R CMD check passing.
5. Hygiene & Scope
   - Diff stays scoped to the linked issue and satisfies all acceptance
     criteria.
   - No hardcoded personal/employer names, credentials, debug logs, `dev/`
     files, or scratch scripts.
   - Conventional Commit messages and `.lintr` compliance (120-char line limit).

REQUIRED OUTPUT FORMAT:
## Verdict: APPROVE | REQUEST_CHANGES

### Summary
2–4 sentences summarizing what the PR does and overall quality.

### Blocking Findings
(Omit section or write "None." if APPROVE)
1. **[BLOCKING] `<path>:<line>` — <short title>**
   - **Why:** <concrete failure mode or violation>
   - **Suggested fix:** <specific code or design change>

### Suggestions & Nits
(Optional non-blocking improvements, clearly labeled `[SUGGESTION]` or `[NIT]`)

### Checklist
- [x] / [ ] Issue acceptance criteria met
- [x] / [ ] Dynamic/static parity preserved
- [x] / [ ] Dependencies & DESCRIPTION/NAMESPACE clean
- [x] / [ ] Offline unit tests & QA checks sufficient
- [x] / [ ] No hygiene/scope issues
```
