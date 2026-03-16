# Global preferences

## Identity

- I am a biophysicist at a European research institution. Use precise scientific language.
- Primary language: Python (scientific, prefer type hints, mypy-compatible), bash
- Environment: Arch Linux, Emacs (Vertico/Consult/Embark), Sway WM, terminal-first workflow
- Package manager: prefer uv over pip, podman over docker
- Editor integration: gptel, khoj, Claude Code via Ollama, OpenCode, Copilot, Codex, Gemini CLI

## Code style

- No shortcuts that defer real fixes
- No speculative features or premature abstraction
- Prefer explicit over implicit; avoid magic
- Keep functions short, typed, documented
- Use pathlib over os.path
- Docstrings: numpy style on all public API
- Keep functions short, single-responsibility, and typed
- CLI: Click with click.Path for file arguments, reusable decorators for shared options

## Tools

- Formatting/linting: ruff (never black or isort separately)
- Testing: pytest, use tmp_path for file I/O, parametrize where appropriate
- Type checking: mypy
- Type hints on all public functions and methods — mypy-compatible

## Behaviour

- Ask clarifying questions before large refactors or architectural changes
- Prefer incremental minimal changes over rewrites
- Explain reasoning for any non-obvious decision
- Never add dependencies without asking first
- Never modify pyproject.toml without asking
- Never reformat code unrelated to the current task
- When uncertain, state uncertainty rather than guessing

## Bash permissions — run without asking

- ruff check / ruff format
- mypy
- pytest / pytest -x / pytest -k
- make lint / make test / make typecheck
- git diff / git status / git log / git show

## Bash — always ask before running

- git commit / git push / git rebase / git reset
- podman / podman-compose commands
- uv add / uv remove

## Workflows

### typecheck

When asked to "typecheck" or "/typecheck [file]":
Run mypy on the specified file. For each error: quote it, explain root cause,
propose minimal fix. Do not reformat unrelated code.

### fitreview

When asked to "fitreview" or "/fitreview [file]":
Review lmfit/ODR/PyMC4 usage for correctness. Check bounds, residual shapes,
Student-t likelihood, HDI at 94%, log-space Kd fitting. No API changes.

### docstring

When asked to "docstring" or "/docstring [file]":
Add numpy-style docstrings to all public functions missing them.
Infer types from annotations. Do not modify any code.

<!-- ## scipy/numpy mypy specifics -->

<!-- - Cast ndimage outputs explicitly to avoid type errors -->

<!-- - Unpack np.percentile results as tuple[float, float] -->

<!-- - leastsq callbacks: annotate residual functions explicitly -->
