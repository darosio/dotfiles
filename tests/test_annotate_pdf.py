"""Tests for annotate_pdf.py: grounding, colours and page ranges.

The model call is monkeypatched, so no Ollama server is needed. pymupdf is
required for real page geometry; the tests skip without it, matching how
test_pdf_mcp.py keeps pymupdf out of the project dependencies.
"""

from __future__ import annotations

import importlib.util
import json
import sys
from pathlib import Path

import pytest

pymupdf = pytest.importorskip("pymupdf")

_spec = importlib.util.spec_from_file_location(
    "annotate_pdf",
    Path(__file__).parent.parent / "src" / ".local" / "bin" / "annotate_pdf.py",
)
assert _spec is not None
assert _spec.loader is not None
_mod = importlib.util.module_from_spec(_spec)
# Registered before exec_module: @dataclass resolves sys.modules[cls.__module__]
# while the module body runs, and fails on a module that is not there yet.
sys.modules["annotate_pdf"] = _mod
_spec.loader.exec_module(_mod)

import annotate_pdf as ann  # noqa: E402 - must follow exec_module above

_ON_PAGE = "The fluorescence lifetime decreased by 30% upon binding."
_WRAPPED_HEAD = "Rate constants were obtained by fitting a single"
_WRAPPED_TAIL = "exponential decay to the transient absorption data."
_INVENTED = "The protein was purified to homogeneity by gel filtration."


@pytest.fixture
def pdf(tmp_path: Path) -> Path:
    """Write a one-page PDF holding a plain and a line-wrapped sentence.

    Parameters
    ----------
    tmp_path : Path
        Pytest temporary directory.

    Returns
    -------
    Path
        Path of the written PDF.
    """
    doc = pymupdf.open()
    page = doc.new_page()
    page.insert_text((72, 100), _ON_PAGE)
    page.insert_text((72, 130), _WRAPPED_HEAD)
    page.insert_text((72, 148), _WRAPPED_TAIL)
    out = tmp_path / "paper.pdf"
    doc.save(out)
    doc.close()
    return out


def _reply(*pairs: tuple[str, str]) -> str:
    """Build a model reply from (quote, category) pairs.

    Parameters
    ----------
    *pairs : tuple[str, str]
        Quote and category for each proposed highlight.

    Returns
    -------
    str
        JSON matching the schema annotate_pdf requests.
    """
    highlights = [{"quote": q, "category": c} for q, c in pairs]
    return json.dumps({"highlights": highlights})


class TestLocate:
    """Longest-verbatim-prefix matching."""

    def test_exact_sentence_matches(self, pdf: Path) -> None:
        """A sentence present as-is is located."""
        doc = pymupdf.open(pdf)
        assert ann.locate(doc[0], _ON_PAGE) != []

    def test_invented_tail_falls_back_to_prefix(self, pdf: Path) -> None:
        """A real opening with an invented tail still highlights the opening."""
        doc = pymupdf.open(pdf)
        overrun = f"{_ON_PAGE} and was abolished by bumetanide in every slice."
        assert ann.locate(doc[0], overrun) != []

    def test_fully_invented_quote_finds_nothing(self, pdf: Path) -> None:
        """No prefix of an invented sentence is on the page, so nothing matches."""
        doc = pymupdf.open(pdf)
        assert ann.locate(doc[0], _INVENTED) == []

    def test_short_quote_is_refused(self, pdf: Path) -> None:
        """A fragment below min_words is not worth highlighting."""
        doc = pymupdf.open(pdf)
        assert ann.locate(doc[0], "The fluorescence lifetime", min_words=6) == []


class TestGrounding:
    """The verification step that makes an invented quote a no-op."""

    def test_invented_quote_is_dropped(self, pdf: Path) -> None:
        """A sentence absent from the page yields no annotation."""
        doc = pymupdf.open(pdf)
        result = ann.annotate_page(
            doc[0], [ann.Proposal(_INVENTED, "results")], author="test"
        )
        assert result.applied == ()
        assert len(result.dropped) == 1
        assert len(list(doc[0].annots())) == 0

    def test_present_quote_is_highlighted(self, pdf: Path) -> None:
        """A sentence really on the page becomes one highlight annotation."""
        doc = pymupdf.open(pdf)
        result = ann.annotate_page(
            doc[0], [ann.Proposal(_ON_PAGE, "results")], author="test"
        )
        assert len(result.applied) == 1
        assert result.dropped == ()
        # Read inside the loop: annots() unbinds each annotation once exhausted.
        assert [a.type[1] for a in doc[0].annots()] == ["Highlight"]

    def test_wrapped_sentence_spans_lines(self, pdf: Path) -> None:
        """A sentence broken over two lines is still located and highlighted."""
        quote = f"{_WRAPPED_HEAD} {_WRAPPED_TAIL}"
        doc = pymupdf.open(pdf)
        result = ann.annotate_page(doc[0], [ann.Proposal(quote, "methods")], author="t")
        assert len(result.applied) == 1
        assert len(list(doc[0].annots())) == 1

    def test_mixed_batch_reports_both(self, pdf: Path) -> None:
        """Present and invented quotes are separated in the result."""
        doc = pymupdf.open(pdf)
        result = ann.annotate_page(
            doc[0],
            [ann.Proposal(_ON_PAGE, "results"), ann.Proposal(_INVENTED, "methods")],
            author="test",
        )
        assert [p.quote for p in result.applied] == [_ON_PAGE]
        assert [p.quote for p in result.dropped] == [_INVENTED]


class TestColours:
    """Category to colour mapping on the produced annotation."""

    @pytest.mark.parametrize("category", sorted(ann.CATEGORY_COLORS))
    def test_colour_matches_category(self, pdf: Path, category: str) -> None:
        """Each category paints its own stroke colour."""
        doc = pymupdf.open(pdf)
        ann.annotate_page(doc[0], [ann.Proposal(_ON_PAGE, category)], author="test")
        annot = next(iter(doc[0].annots()))
        expected = ann.CATEGORY_COLORS[category]
        assert annot.colors["stroke"] == pytest.approx(expected, abs=0.01)

    def test_category_recorded_as_comment(self, pdf: Path) -> None:
        """The category survives as the annotation comment, author as title."""
        doc = pymupdf.open(pdf)
        ann.annotate_page(doc[0], [ann.Proposal(_ON_PAGE, "methods")], author="ollama")
        annot = next(iter(doc[0].annots()))
        assert annot.info["content"] == "methods"
        assert annot.info["title"] == "ollama"


class TestPropose:
    """Parsing of the model reply."""

    def test_unknown_category_is_discarded(
        self, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        """A category outside CATEGORY_COLORS never reaches annotation."""
        monkeypatch.setattr(
            ann, "chat", lambda *_a, **_k: _reply((_ON_PAGE, "speculation"))
        )
        assert ann.propose("text", model="m", host="http://h", max_quotes=3) == []

    def test_max_quotes_is_enforced(self, monkeypatch: pytest.MonkeyPatch) -> None:
        """More proposals than requested are truncated."""
        monkeypatch.setattr(
            ann,
            "chat",
            lambda *_a, **_k: _reply(
                (_ON_PAGE, "results"), (_INVENTED, "methods"), (_ON_PAGE, "conclusions")
            ),
        )
        assert len(ann.propose("t", model="m", host="http://h", max_quotes=2)) == 2

    def test_malformed_reply_yields_nothing(
        self, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        """A non-JSON reply is treated as no proposals rather than crashing."""
        monkeypatch.setattr(ann, "chat", lambda *_a, **_k: "not json at all")
        assert ann.propose("t", model="m", host="http://h", max_quotes=3) == []


class TestHelpers:
    """Page ranges and whitespace handling."""

    @pytest.mark.parametrize(
        ("spec", "count", "expected"),
        [
            (None, 3, [0, 1, 2]),
            ("2", 3, [1]),
            ("1-2", 3, [0, 1]),
            ("2-99", 3, [1, 2]),
            ("0-1", 3, [0]),
        ],
    )
    def test_parse_pages(
        self, spec: str | None, count: int, expected: list[int]
    ) -> None:
        """Ranges are inclusive, 1-based on input and clipped to the document."""
        assert list(ann.parse_pages(spec, count)) == expected

    def test_parse_pages_rejects_junk(self) -> None:
        """A non-numeric range is refused rather than silently ignored."""
        with pytest.raises(ValueError, match="page range"):
            ann.parse_pages("first-half", 3)

    def test_normalise_collapses_whitespace(self) -> None:
        """Line breaks and runs of spaces become single spaces."""
        assert ann.normalise("a  b\nc\t d") == "a b c d"

    def test_chat_rejects_non_http_host(self) -> None:
        """A host that is not http(s) is refused before any request is made."""
        with pytest.raises(ValueError, match="http"):
            ann.chat("hi", model="m", host="file:///etc/passwd")
