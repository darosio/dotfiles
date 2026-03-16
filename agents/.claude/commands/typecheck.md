# typecheck.md

# Usage: /typecheck [file or module]

# Invoke as: /typecheck src/chloride_fitting/models.py

Run mypy on $ARGUMENTS using the project's mypy configuration.
For each reported error:

1. Quote the exact error message and line
2. Explain the root cause in plain language
3. Propose the minimal fix consistent with existing type annotations
4. Note if the fix requires changes elsewhere (e.g. call sites)

Do not reformat or refactor code beyond what is needed to fix the type errors.
Do not change function signatures unless the signature itself is the source of the error.
