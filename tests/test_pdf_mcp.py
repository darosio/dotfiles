"""Tests for pdf-mcp.py: zotero_lookup.

Uses a minimal in-memory .bib fixture so tests run without Zotero or pymupdf.
"""

from __future__ import annotations

import importlib.util
import sys
import textwrap
from pathlib import Path
from typing import TYPE_CHECKING
from unittest.mock import MagicMock

if TYPE_CHECKING:
    import pytest

# pdf-mcp.py imports pymupdf at module level — stub it so tests run without it.
_stub = MagicMock()
sys.modules.setdefault("pymupdf", _stub)
sys.modules.setdefault("fitz", _stub)

_scripts_path = str(Path(__file__).parent.parent / "emacs" / ".local" / "bin")

_spec = importlib.util.spec_from_file_location(
    "pdf_mcp", Path(_scripts_path) / "pdf-mcp.py"
)
assert _spec is not None
assert _spec.loader is not None
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)
sys.modules["pdf_mcp"] = _mod

import pdf_mcp as mcp  # type: ignore[import-not-found]  # noqa: E402

_BIB = textwrap.dedent("""\
    @article{Vaswani2017_AV,
      author = {Vaswani, Ashish},
      title  = {Attention Is All You Need},
      doi    = {10.48550/arXiv.1706.03762},
      file   = {/home/dan/Sync/biblio/main/Vaswani2017_AV.pdf},
    }

    @article{Smith2023_JS,
      author = {Smith, Jane},
      title  = {Calcium imaging in zebrafish},
      doi    = {10.1038/s41593-023-01234-5},
      file   = {/home/dan/Sync/biblio/main/Smith2023_JS.pdf},
    }
""")


class TestZoteroLookup:
    """Tests for zotero_lookup()."""

    def test_found_by_doi(self, tmp_path: Path) -> None:
        """Test zotero_lookup successfully finds entry by DOI."""
        bib = tmp_path / "main.bib"
        bib.write_text(_BIB)
        result = mcp.zotero_lookup(doi="10.48550/arXiv.1706.03762", bib_path=bib)
        assert result["found"] is True
        assert result["key"] == "Vaswani2017_AV"
        assert "@article" in result["bibtex"]

    def test_found_by_doi_with_url_prefix(self, tmp_path: Path) -> None:
        """Test zotero_lookup finds entry when DOI has https://doi.org/ prefix."""
        bib = tmp_path / "main.bib"
        bib.write_text(_BIB)
        result = mcp.zotero_lookup(
            doi="https://doi.org/10.48550/arXiv.1706.03762", bib_path=bib
        )
        assert result["found"] is True
        assert result["key"] == "Vaswani2017_AV"

    def test_found_by_filename(self, tmp_path: Path) -> None:
        """Test zotero_lookup successfully finds entry by PDF filename."""
        bib = tmp_path / "main.bib"
        bib.write_text(_BIB)
        result = mcp.zotero_lookup(filename="Smith2023_JS.pdf", bib_path=bib)
        assert result["found"] is True
        assert result["key"] == "Smith2023_JS"

    def test_not_found(self, tmp_path: Path) -> None:
        """Test zotero_lookup returns found=False when DOI is not in bibliography."""
        bib = tmp_path / "main.bib"
        bib.write_text(_BIB)
        result = mcp.zotero_lookup(doi="10.9999/does-not-exist", bib_path=bib)
        assert result["found"] is False

    def test_missing_bib_file(self, tmp_path: Path) -> None:
        """Test zotero_lookup returns error when bibliography file does not exist."""
        result = mcp.zotero_lookup(doi="10.1234/x", bib_path=tmp_path / "missing.bib")
        assert result["found"] is False
        assert "not found" in result["error"].lower()

    def test_no_args_returns_not_found(self, tmp_path: Path) -> None:
        """Test zotero_lookup returns found=False when called without doi or filename.

        Ensures the function handles the no-args case gracefully.
        """
        bib = tmp_path / "main.bib"
        bib.write_text(_BIB)
        result = mcp.zotero_lookup(bib_path=bib)
        assert result["found"] is False

    def test_multi_bib_found_in_second_file(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        """Test zotero_lookup finds an entry present only in the second bib file."""
        bib1 = tmp_path / "main.bib"
        bib1.write_text(_BIB)
        bib2 = tmp_path / "MY.bib"
        bib2.write_text(
            textwrap.dedent("""\
            @article{Jones2020_MJ,
              author = {Jones, Mary},
              title  = {Chloride homeostasis},
              doi    = {10.1016/j.celrep.2020.01.001},
              file   = {/home/dan/Sync/biblio/MY/Jones2020_MJ.pdf},
            }
        """)
        )
        monkeypatch.setenv("ZOTERO_BIB_FILES", f"{bib1}:{bib2}")
        result = mcp.zotero_lookup(doi="10.1016/j.celrep.2020.01.001")
        assert result["found"] is True
        assert result["key"] == "Jones2020_MJ"

    def test_multi_bib_not_found_in_any(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        """Test zotero_lookup returns found=False when DOI absent from all bib files."""
        bib1 = tmp_path / "main.bib"
        bib1.write_text(_BIB)
        bib2 = tmp_path / "MY.bib"
        bib2.write_text(_BIB)
        monkeypatch.setenv("ZOTERO_BIB_FILES", f"{bib1}:{bib2}")
        result = mcp.zotero_lookup(doi="10.9999/absent")
        assert result["found"] is False
