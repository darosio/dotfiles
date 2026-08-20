"""Tests for annotate_pdf.py: grounding, colours and page ranges.

The model call is monkeypatched, so no Ollama server is needed. pymupdf is
required for real page geometry; the tests skip without it, matching how
test_pdf_mcp.py keeps pymupdf out of the project dependencies.
"""

from __future__ import annotations

import email.message
import importlib.util
import io
import json
import shutil
import subprocess  # noqa: S404 - patched, never executed in tests
import sys
import urllib.error
import urllib.request
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
        assert (
            ann.propose(
                "text", provider="ollama", model="m", host="http://h", max_quotes=3
            )
            == []
        )

    def test_max_quotes_is_enforced(self, monkeypatch: pytest.MonkeyPatch) -> None:
        """More proposals than requested are truncated."""
        monkeypatch.setattr(
            ann,
            "chat",
            lambda *_a, **_k: _reply(
                (_ON_PAGE, "results"), (_INVENTED, "methods"), (_ON_PAGE, "conclusions")
            ),
        )
        assert (
            len(
                ann.propose(
                    "t", provider="ollama", model="m", host="http://h", max_quotes=2
                )
            )
            == 2
        )

    def test_malformed_reply_yields_nothing(
        self, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        """A non-JSON reply is treated as no proposals rather than crashing."""
        monkeypatch.setattr(ann, "chat", lambda *_a, **_k: "not json at all")
        assert (
            ann.propose(
                "t", provider="ollama", model="m", host="http://h", max_quotes=3
            )
            == []
        )


class TestHttpErrors:
    """A provider refusal reads as a sentence, not a traceback."""

    @staticmethod
    def _error(body: bytes) -> urllib.error.HTTPError:
        """Build an HTTPError carrying *body*.

        Parameters
        ----------
        body : bytes
            Response payload.

        Returns
        -------
        urllib.error.HTTPError
            Error with the given body.
        """
        return urllib.error.HTTPError(
            "https://x",
            429,
            "Too Many Requests",
            email.message.Message(),
            io.BytesIO(body),
        )

    def test_google_wraps_its_error_in_a_list(self) -> None:
        """Google returns a JSON array; the message is still found."""
        body = b'[{"error": {"message": "Your prepayment credits are depleted."}}]'
        assert "prepayment" in ann.http_error_message(self._error(body))

    def test_bare_object_shape(self) -> None:
        """An Ollama-style bare error object is unwrapped too."""
        body = b'{"error": {"message": "model not found"}}'
        assert ann.http_error_message(self._error(body)) == "model not found"

    def test_non_json_body_falls_back_to_reason(self) -> None:
        """A gateway HTML page degrades to the HTTP reason."""
        assert ann.http_error_message(self._error(b"<html>502</html>")) == (
            "Too Many Requests"
        )

    def test_timeout_becomes_runtime_error(
        self, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        """A stalled read is reported, not raised as a bare TimeoutError."""

        def stall(*_a: object, **_k: object) -> object:
            msg = "The read operation timed out"
            raise TimeoutError(msg)

        monkeypatch.setattr(urllib.request, "urlopen", stall)
        with pytest.raises(RuntimeError, match="did not answer"):
            ann.chat("p", provider="deepseek", model="m", host="http://h")

    def test_url_error_becomes_runtime_error(
        self, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        """A refused connection is reported the same way."""

        def refuse(*_a: object, **_k: object) -> object:
            msg = "connection refused"
            raise urllib.error.URLError(msg)

        monkeypatch.setattr(urllib.request, "urlopen", refuse)
        with pytest.raises(RuntimeError, match="did not answer"):
            ann.chat("p", provider="ollama", model="m", host="http://h")

    def test_chat_raises_with_provider_message(
        self, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        """chat() turns an HTTPError into a RuntimeError carrying the detail."""
        body = b'[{"error": {"message": "credits depleted"}}]'

        def boom(*_a: object, **_k: object) -> object:
            raise self._error(body)

        monkeypatch.setattr(urllib.request, "urlopen", boom)
        with pytest.raises(RuntimeError, match="credits depleted"):
            ann.chat("p", provider="ollama", model="m", host="http://h")


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
            ann.chat("hi", provider="ollama", model="m", host="file:///etc/passwd")


class TestProviders:
    """Request shape and response parsing per provider."""

    def test_ollama_request_disables_thinking(self) -> None:
        """The Ollama body carries think=False and the schema in format."""
        url, payload, headers = ann.build_request(
            "p", provider="ollama", model="m", host="http://localhost:11434"
        )
        body = json.loads(payload)
        assert url == "http://localhost:11434/api/chat"
        assert body["think"] is False
        assert body["format"]["required"] == ["highlights"]
        assert "Authorization" not in headers

    def test_gemini_request_uses_openai_shape(
        self, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        """The Gemini body is OpenAI-compatible and bearer-authenticated."""
        monkeypatch.setenv("GEMINI_API_KEY", "secret")
        url, payload, headers = ann.build_request(
            "p", provider="gemini", model="gemini-3.6-flash", host="https://x/openai"
        )
        body = json.loads(payload)
        assert url == "https://x/openai/chat/completions"
        assert headers["Authorization"] == "Bearer secret"
        schema = body["response_format"]["json_schema"]["schema"]
        assert schema["required"] == ["highlights"]
        assert "think" not in body

    @pytest.mark.parametrize(
        ("provider", "body", "expected"),
        [
            ("ollama", {"message": {"content": "x"}}, "x"),
            ("gemini", {"choices": [{"message": {"content": "y"}}]}, "y"),
        ],
    )
    def test_extract_content(
        self, provider: str, body: dict[str, object], expected: str
    ) -> None:
        """Each provider's reply is unwrapped from its own envelope."""
        assert ann.extract_content(provider, body) == expected

    def test_key_prefers_environment(self, monkeypatch: pytest.MonkeyPatch) -> None:
        """An exported key is used without shelling out to pass."""
        monkeypatch.setenv("GEMINI_API_KEY", "from-env")
        assert ann.api_key("gemini") == "from-env"

    def test_key_falls_back_to_pass(self, monkeypatch: pytest.MonkeyPatch) -> None:
        """With no env key the pass entry is consulted."""
        monkeypatch.delenv("GEMINI_API_KEY", raising=False)
        monkeypatch.delenv("GOOGLE_API_KEY", raising=False)
        monkeypatch.setattr(shutil, "which", lambda _: "/usr/bin/pass")
        monkeypatch.setattr(
            subprocess,
            "run",
            lambda *_a, **_k: type("R", (), {"stdout": "from-pass\nother\n"})(),
        )
        assert ann.api_key("gemini") == "from-pass"

    def test_pass_lookup_sets_store_dir(self, monkeypatch: pytest.MonkeyPatch) -> None:
        """The pass call carries PASSWORD_STORE_DIR; this store is not default."""
        monkeypatch.delenv("GEMINI_API_KEY", raising=False)
        monkeypatch.delenv("GOOGLE_API_KEY", raising=False)
        monkeypatch.delenv("PASSWORD_STORE_DIR", raising=False)
        monkeypatch.setattr(shutil, "which", lambda _: "/usr/bin/pass")
        seen: dict[str, str] = {}

        def fake_run(*_a: object, **kwargs: object) -> object:
            env = kwargs["env"]
            assert isinstance(env, dict)
            seen.update(env)
            return type("R", (), {"stdout": "k\n"})()

        monkeypatch.setattr(subprocess, "run", fake_run)
        assert ann.api_key("gemini") == "k"
        assert seen["PASSWORD_STORE_DIR"].endswith("/Sync/.pass")

    def test_existing_store_dir_is_respected(
        self, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        """An exported PASSWORD_STORE_DIR is not overridden."""
        monkeypatch.delenv("GEMINI_API_KEY", raising=False)
        monkeypatch.delenv("GOOGLE_API_KEY", raising=False)
        monkeypatch.setenv("PASSWORD_STORE_DIR", "/custom/store")
        monkeypatch.setattr(shutil, "which", lambda _: "/usr/bin/pass")
        seen: dict[str, str] = {}

        def fake_run(*_a: object, **kwargs: object) -> object:
            env = kwargs["env"]
            assert isinstance(env, dict)
            seen.update(env)
            return type("R", (), {"stdout": "k\n"})()

        monkeypatch.setattr(subprocess, "run", fake_run)
        ann.api_key("gemini")
        assert seen["PASSWORD_STORE_DIR"] == "/custom/store"  # noqa: S105

    def test_missing_key_is_explicit(self, monkeypatch: pytest.MonkeyPatch) -> None:
        """No key anywhere raises rather than sending an unauthenticated call."""
        monkeypatch.delenv("GEMINI_API_KEY", raising=False)
        monkeypatch.delenv("GOOGLE_API_KEY", raising=False)
        monkeypatch.setattr(shutil, "which", lambda _: None)
        with pytest.raises(ValueError, match="no gemini API key"):
            ann.api_key("gemini")

    def test_deepseek_uses_json_object_not_schema(
        self, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        """DeepSeek rejects json_schema, so only json_object is sent."""
        monkeypatch.setenv("DEEPSEEK_API_KEY", "ds")
        _url, payload, headers = ann.build_request(
            "p", provider="deepseek", model="deepseek-v4-flash", host="https://d/v1"
        )
        body = json.loads(payload)
        assert body["response_format"] == {"type": "json_object"}
        assert headers["Authorization"] == "Bearer ds"

    def test_prompt_says_json_for_deepseek(
        self, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        """The word 'json' must reach the prompt or DeepSeek returns nothing."""
        captured: dict[str, str] = {}

        def fake_chat(prompt: str, **_k: object) -> str:
            captured["prompt"] = prompt
            return _reply()

        monkeypatch.setattr(ann, "chat", fake_chat)
        ann.propose("t", provider="deepseek", model="m", host="http://h", max_quotes=1)
        assert "json" in captured["prompt"]

    def test_openai_shape_shared_by_hosted_providers(self) -> None:
        """Gemini and DeepSeek both answer in the choices envelope."""
        body = {"choices": [{"message": {"content": "z"}}]}
        assert ann.extract_content("deepseek", body) == "z"
        assert ann.extract_content("gemini", body) == "z"

    def test_provider_defaults_are_distinct(self) -> None:
        """Each provider carries its own model and endpoint default."""
        assert ann.DEFAULT_HOSTS["ollama"].startswith("http://localhost")
        assert "generativelanguage" in ann.DEFAULT_HOSTS["gemini"]
        assert set(ann.DEFAULT_MODELS) == set(ann.PROVIDERS)
