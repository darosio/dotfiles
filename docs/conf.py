"""Sphinx configuration for dotfiles documentation."""

project = "dotfiles"
author = "Daniele Arosio"
copyright = f"2024, {author}"  # noqa: A001

extensions = [
    "autoapi.extension",
    "myst_parser",
    "sphinx.ext.napoleon",
    "sphinx_copybutton",
]

myst_enable_extensions = [
    "colon_fence",
    "deflist",
    "fieldlist",
]

autodoc_typehints = "description"  # signature(default), combined

# -- sphinx-autoapi configuration --------------------------------------------
autoapi_dirs = ["../src"]
autoapi_ignore = ["**/.ipynb_checkpoints/*"]
autoapi_options = [
    "members",
    "show-inheritance",
    "show-module-summary",
]
autoapi_member_order = "bysource"
autoapi_add_toctree_entry = True
autoapi_keep_files = True

templates_path = ["_templates"]
exclude_patterns = ["_build", "Thumbs.db", ".DS_Store"]

html_theme = "pydata_sphinx_theme"
html_title = "dotfiles"

# Source file suffixes
source_suffix = {
    ".rst": "restructuredtext",
    ".md": "markdown",
}
