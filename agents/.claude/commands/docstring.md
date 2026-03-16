# docstring.md

# Usage: /docstring [file]

# Invoke as: /docstring src/chloride_fitting/models.py

Add numpy-style docstrings to all public functions and classes in $ARGUMENTS
that are currently missing them or have incomplete docstrings.

Format:

```
Short one-line summary.

Extended description if the function is non-trivial.

Parameters
----------
name : type
    Description.

Returns
-------
type
    Description.

Raises
------
ExceptionType
    When this is raised.

Notes
-----
Any mathematical or algorithmic notes, e.g. fitting conventions,
parameter space transformations, or references to literature.
```

Rules:

- Infer parameter types from existing annotations — do not guess
- Do not document private functions (prefixed with \_) unless asked
- Do not change any code, only add/complete docstrings
