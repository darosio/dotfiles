"""Sphinx configuration for dotfiles documentation."""

project = "dotfiles"
author = "Daniele Arosio"
copyright = f"2024, {author}"  # noqa: A001

extensions = [
    "myst_parser",
    "autodocsumm",
    "sphinx.ext.autodoc",
    "sphinx.ext.napoleon",
    "sphinx_autodoc_typehints",
    "sphinx_copybutton",
]

myst_enable_extensions = [
    "colon_fence",
    "deflist",
    "fieldlist",
]

templates_path = ["_templates"]
exclude_patterns = ["_build", "Thumbs.db", ".DS_Store"]

html_theme = "pydata_sphinx_theme"
html_title = "dotfiles"

# Source file suffixes
source_suffix = {
    ".rst": "restructuredtext",
    ".md": "markdown",
}
