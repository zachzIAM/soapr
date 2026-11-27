# soapr — Agent Guidelines

`soapr` is an R package (inspired by Python's `zeep`) for interacting with SOAP web services via both a **dynamic runtime client** and a **static R package generator** driven by WSDL definitions.

## Repository Map

- `R/WSDLParser.R` — Parses WSDL 1.1/XSD definitions (services, ports, bindings, operations, complex/simple/element types, schema form defaults, documentation).
- `R/Factory.R` — Instantiates `soap_object` instances and deserializes XML responses into R objects.
- `R/XMLSerializer.R` — Serializes R objects / `soap_object`s to XML respecting `elementFormDefault` and `attributeFormDefault`, and parses XML HTTP responses.
- `R/Client.R` & `R/Service.R` — Dynamic SOAP client and service/port selector with pluggable HTTP transport and SOAP 1.1/1.2 fault handling.
- `R/WSSecurity.R` — WS-Security (`UsernameToken`) SOAP header helper.
- `R/Generator.R` & `inst/templates/` — Generates standalone R packages (`DESCRIPTION`, `R/client.R`, `R/types.R`, `R/schema.R`, `R/utils.R`, `R/detached_docs.R`, `README.md`) via `whisker`.
- `tests/testthat/` — Unit tests (mocked transports + local WSDLs in `testdata/`) and live-service integration tests (`test-live-*.R`).
- `.github/workflows/` — CI (`check.yaml`), documentation site (`pkgdown.yaml`), and release automation.

## Workflow (GitHub Flow)

- **`main` is always releasable.** Never commit directly to `main` for feature or bug work.
- **Branch naming:** `<type>/<issue-number>-<short-slug>` (e.g., `fix/12-soap12-content-type`, `feat/15-xsd-imports`).
- **Commits:** Use [Conventional Commits](https://www.conventionalcommits.org/) (`feat:`, `fix:`, `docs:`, `test:`, `refactor:`, `chore:`) to support automated versioning and release notes.
- **Role split:**
  - **Planner / Triage:** Investigates issues, designs fixes, and creates clearly scoped GitHub Issues with acceptance criteria, affected files, and labels.
  - **Implementer:** Works on a single assigned issue from a fresh branch off `main`, writes tests first or alongside the change, runs all QA checks, opens a PR with `Closes #<number>`, runs the local `pr-reviewer` skill (`.agents/skills/pr-reviewer/SKILL.md`) in an independent read-only subagent to address any blocking findings, and merges only after the review approves and CI (`R-CMD-check`) is green.

## Coding & Package Standards

- **Style & Linting:** Follow `.lintr` (120-char line limit; `snake_case`, `CamelCase`, `camelCase`, `dotted.case` allowed). Run `lintr::lint_package()` before committing.
- **Dependencies:** Avoid data-frame/tabular tidyverse packages (`dplyr`, `tidyr`, `tibble`, `readr`, `ggplot2`, etc.) and keep the dependency footprint lean — do not add a package just for one or two helper functions (prefer base R unless a package provides substantial value across the codebase). Lean type-safety / r-lib-style packages that earn their keep broadly across `soapr` (such as `purrr`) are acceptable in `soapr` itself, while generated static packages (`inst/templates/DESCRIPTION.mustache`) must stay minimal (`httr`, `xml2`, `R6`, `bit64`). Always use explicit `pkg::fn()` qualification. Every package referenced in `R/` must be declared in `DESCRIPTION` (`Imports:` or `Suggests:`). Keep `NAMESPACE` minimal via `R/soapr-package.R`.
- **Tooling:** R versions are managed via `rig`. GitHub CLI (`gh`) authentication is managed through 1Password CLI (`op`).
- **Parity between dynamic and static clients:** Changes to serialization, deserialization, fault handling, or type coercion in `R/XMLSerializer.R`, `R/Factory.R`, or `R/Client.R` must be checked against `inst/templates/utils.R`, `inst/templates/client.mustache`, and `inst/templates/types.mustache` (and vice versa) so dynamic and generated clients do not drift.
- **No hardcoded metadata:** Never hardcode personal or employer names, credentials, or service-specific hacks into `R/` or `inst/templates/`.
- **Clean tree:** Do not commit scratch scripts, `dev/` logs, `generated_packages/`, or debug XML dumps.

## Testing & Verification

Before opening or merging a PR, verify:

1. **Documentation is up to date:** `roxygen2::roxygenise()` (no unstaged diffs in `man/` or `NAMESPACE`).
2. **Offline unit tests pass:** `testthat::test_local()` — all core tests must use local WSDL fixtures and mock transports so CI is deterministic and fast.
3. **Live tests are properly gated:** Any test hitting an external endpoint (`test-live-*.R`, `test-integration-static-clients.R`) must guard against offline/flaky environments (e.g., `skip_on_cran()`, `skip_if_offline()`, or an opt-in env var like `SOAPR_RUN_LIVE_TESTS=true`).
4. **Package check passes cleanly:** `rcmdcheck::rcmdcheck(args = c("--no-manual"))` with 0 errors and 0 warnings.
5. **Linting:** `lintr::lint_package()` reports no new issues.
