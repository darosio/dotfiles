#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.13"
# dependencies = ["click", "pymupdf"]
# ///
"""Highlight key sentences in a PDF with a local Ollama model.

The model proposes verbatim sentences with a category; each proposal is then
located in the page with pymupdf's ``search_for`` and only becomes a highlight
if the text is really there. A sentence the model invented is therefore dropped
rather than annotated: hallucination degrades to a missing highlight, never a
wrong one.

Highlights are written into the PDF itself, so Zotero 7 renders them in its
reader and can import them as native annotations.

Dependencies are declared inline (PEP 723) and resolved by uv on first run, so
the script works straight off PATH without being added to any environment:

Usage:
  annotate_pdf.py --dry-run paper.pdf
  annotate_pdf.py --pages 1-8 paper.pdf
  annotate_pdf.py --provider gemini paper.pdf
  annotate_pdf.py --in-place ~/Zotero/storage/ABCD1234/paper.pdf

Editing a file under ~/Zotero/storage makes Zotero re-upload it on the next
sync, which is why --in-place is opt-in and the default writes a copy.

Thinking models (qwen3.5, qwen3) spend their whole token budget on reasoning
and return empty content, so ``think`` is disabled in the request.

Speed is set by the model, and quality here means quoting verbatim rather than
paraphrasing: on one paper page (4000 chars) qwen3.5:4b took 5.5 min on 8 CPU
cores and quoted exactly, while qwen3:1.7b took 41 s and paraphrased, so its
proposal was dropped by the verification step. Prefer the larger model and run
it over a whole paper in the background; ``ollama ps`` will tell you whether it
is on CPU, which is what makes this slow.

--provider gemini trades that wait for money, through Google's OpenAI-compatible
endpoint. At the measured 1116 input and 265 output tokens per page, 3.6 Flash
costs about $0.0018 a page: roughly $0.04 for a 20-page paper. It is billed from
the first token once billing is enabled on the project, so keep the prepay
balance low and auto-reload off. The key is read from GEMINI_API_KEY, or from
`pass cloud/gemini_API_key` if that is unset, so nothing needs exporting.
"""

from __future__ import annotations

import json
import os
import shutil
import subprocess
import urllib.request
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any, Final

import click

try:
    import pymupdf  # type: ignore[import-not-found]  # pymupdf >= 1.24
except ImportError:  # pragma: no cover - older wheels expose the fitz name
    import fitz as pymupdf  # type: ignore[import-not-found]

if TYPE_CHECKING:
    from collections.abc import Iterable, Sequence

# Beaver's convention, and a sane one: one colour per kind of claim.
CATEGORY_COLORS: Final[dict[str, tuple[float, float, float]]] = {
    "methods": (0.34, 0.61, 0.95),  # blue
    "results": (0.98, 0.83, 0.25),  # yellow
    "conclusions": (0.42, 0.79, 0.47),  # green
}

PROVIDERS: Final = ("ollama", "gemini")
DEFAULT_MODELS: Final[dict[str, str]] = {
    "ollama": "qwen3.5:4b",
    "gemini": "gemini-3.6-flash",
}
DEFAULT_HOSTS: Final[dict[str, str]] = {
    "ollama": "http://localhost:11434",
    # Google's OpenAI-compatible surface, so one request shape serves both.
    "gemini": "https://generativelanguage.googleapis.com/v1beta/openai",
}
_KEY_ENV_VARS: Final = ("GEMINI_API_KEY", "GOOGLE_API_KEY")
_GEMINI_STORE_ENTRY: Final = "cloud/gemini_API_key"
_MAX_PAGE_CHARS: Final = 4000  # ~1100 prompt tokens, measured on a paper page
_MAX_TOKENS: Final = 512  # three quoted sentences need ~270
_MIN_MATCH_WORDS: Final = 6  # shorter fragments match anything and mean little
_TIMEOUT_S: Final = 900  # a dense page took 5.5 min on 8 CPU cores

_SYSTEM: Final = (
    "You select sentences to highlight in a scientific paper. "
    "Copy sentences EXACTLY as they appear, character for character. "
    "Never paraphrase, never join fragments, never fix typos. "
    "Prefer sentences stating what was measured, what was found, or what it "
    "means. Return no sentence at all rather than an approximate one."
)

_SCHEMA: Final[dict[str, Any]] = {
    "type": "object",
    "properties": {
        "highlights": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "quote": {"type": "string"},
                    "category": {
                        "type": "string",
                        "enum": sorted(CATEGORY_COLORS),
                    },
                },
                "required": ["quote", "category"],
            },
        },
    },
    "required": ["highlights"],
}


@dataclass(frozen=True)
class Proposal:
    """A sentence the model proposed for highlighting.

    Attributes
    ----------
    quote : str
        The sentence, expected verbatim from the page.
    category : str
        One of the keys of :data:`CATEGORY_COLORS`.
    """

    quote: str
    category: str


@dataclass(frozen=True)
class PageResult:
    """Outcome of annotating one page.

    Attributes
    ----------
    number : int
        1-based page number.
    applied : tuple of Proposal
        Proposals found in the page and highlighted.
    dropped : tuple of Proposal
        Proposals whose text could not be located, and so were discarded.
    """

    number: int
    applied: tuple[Proposal, ...]
    dropped: tuple[Proposal, ...]


def normalise(quote: str) -> str:
    """Collapse whitespace so a line-wrapped sentence can still be matched.

    Parameters
    ----------
    quote : str
        Sentence as returned by the model.

    Returns
    -------
    str
        The same characters with runs of whitespace reduced to single spaces.
    """
    return " ".join(quote.split())


def locate(
    page: Any,  # noqa: ANN401 - pymupdf.Page; the package ships no stubs
    quote: str,
    *,
    min_words: int = _MIN_MATCH_WORDS,
) -> list[Any]:
    """Find the longest verbatim prefix of *quote* on *page*.

    A model often gets a sentence right and then runs past the end of it, or
    into a column break or an extraction artifact. Trimming words from the tail
    recovers those cases while keeping the guarantee that matters: whatever is
    highlighted is text that is demonstrably on the page.

    Parameters
    ----------
    page : Any
        A ``pymupdf.Page``; typed loosely because pymupdf ships no stubs.
    quote : str
        Sentence as returned by the model.
    min_words : int
        Shortest prefix worth highlighting; below this a match says little.

    Returns
    -------
    list[Any]
        Quads covering the matched prefix, empty when nothing matches.
    """
    words = normalise(quote).split()
    for count in range(len(words), min_words - 1, -1):
        quads = page.search_for(" ".join(words[:count]), quads=True)
        if quads:
            return list(quads)
    return []


def api_key() -> str:
    """Return the Gemini API key from the environment or from pass.

    Falls back to ``pass`` because that is where the rest of this setup keeps
    the key (my-ai.el and hermes-secrets both read the same entry), so no key
    has to be exported into the shell to use this script.

    Returns
    -------
    str
        The API key.

    Raises
    ------
    ValueError
        If no key is found in either place.
    """
    for var in _KEY_ENV_VARS:
        value = os.environ.get(var)
        if value:
            return value
    pass_bin = shutil.which("pass")
    if pass_bin:
        # This store does not live at pass's default path; mirror the fallback
        # hermes-secrets uses so the key resolves without exporting anything.
        env = dict(os.environ)
        env.setdefault("PASSWORD_STORE_DIR", str(Path.home() / "Sync" / ".pass"))
        found = subprocess.run(  # noqa: S603 - fixed argv, path from shutil.which
            [pass_bin, "show", _GEMINI_STORE_ENTRY],
            capture_output=True,
            text=True,
            check=False,
            env=env,
        )
        key = found.stdout.split("\n", 1)[0].strip()
        if key:
            return key
    msg = (
        f"no Gemini API key: set {_KEY_ENV_VARS[0]} or store it at "
        f"`pass {_GEMINI_STORE_ENTRY}`"
    )
    raise ValueError(msg)


def build_request(
    prompt: str, *, provider: str, model: str, host: str
) -> tuple[str, bytes, dict[str, str]]:
    """Build the HTTP request for *provider*.

    Both providers take the same messages and the same JSON schema; they differ
    in where the schema goes, how generation is capped, and how they authenticate.

    Parameters
    ----------
    prompt : str
        User message; the system prompt is added here.
    provider : str
        One of :data:`PROVIDERS`.
    model : str
        Model name.
    host : str
        Base URL for the provider.

    Returns
    -------
    tuple[str, bytes, dict[str, str]]
        URL, encoded payload and headers.
    """
    messages = [
        {"role": "system", "content": _SYSTEM},
        {"role": "user", "content": prompt},
    ]
    headers = {"Content-Type": "application/json"}
    if provider == "gemini":
        payload: dict[str, Any] = {
            "model": model,
            "messages": messages,
            "max_tokens": _MAX_TOKENS,
            "response_format": {
                "type": "json_schema",
                "json_schema": {"name": "highlights", "schema": _SCHEMA},
            },
        }
        headers["Authorization"] = f"Bearer {api_key()}"
        return f"{host}/chat/completions", json.dumps(payload).encode(), headers
    payload = {
        "model": model,
        "stream": False,
        "think": False,  # thinking models otherwise return empty content
        "format": _SCHEMA,
        "options": {"num_predict": _MAX_TOKENS},
        "messages": messages,
    }
    return f"{host}/api/chat", json.dumps(payload).encode(), headers


def extract_content(provider: str, body: dict[str, Any]) -> str:
    """Pull the assistant text out of a provider's response body.

    Parameters
    ----------
    provider : str
        One of :data:`PROVIDERS`.
    body : dict[str, Any]
        Decoded JSON response.

    Returns
    -------
    str
        Assistant content, expected to be JSON matching :data:`_SCHEMA`.
    """
    if provider == "gemini":
        return str(body["choices"][0]["message"]["content"])
    return str(body["message"]["content"])


def chat(
    prompt: str,
    *,
    provider: str,
    model: str,
    host: str,
    timeout: int = _TIMEOUT_S,
) -> str:
    """Send *prompt* to an Ollama chat endpoint and return the reply content.

    Parameters
    ----------
    prompt : str
        User message; the system prompt is added here.
    provider : str
        One of :data:`PROVIDERS`.
    model : str
        Model name.
    host : str
        Base URL for the provider.
    timeout : int
        Seconds to wait for the response.

    Returns
    -------
    str
        Raw assistant content, expected to be JSON matching :data:`_SCHEMA`.

    Raises
    ------
    ValueError
        If *host* is not an http(s) URL.
    """
    if not host.startswith(("http://", "https://")):
        msg = f"host must be an http(s) URL, got {host!r}"
        raise ValueError(msg)
    url, payload, headers = build_request(
        prompt, provider=provider, model=model, host=host
    )
    request = urllib.request.Request(url, data=payload, headers=headers)  # noqa: S310 - scheme checked above
    with urllib.request.urlopen(request, timeout=timeout) as response:  # noqa: S310
        body = json.loads(response.read())
    return extract_content(provider, body)


def propose(  # noqa: PLR0913
    text: str,
    *,
    provider: str,
    model: str,
    host: str,
    max_quotes: int,
    timeout: int = _TIMEOUT_S,
) -> list[Proposal]:
    """Ask the model which sentences of *text* deserve a highlight.

    Parameters
    ----------
    text : str
        Page text, truncated to :data:`_MAX_PAGE_CHARS`.
    provider : str
        One of :data:`PROVIDERS`.
    model : str
        Model name.
    host : str
        Base URL for the provider.
    max_quotes : int
        Upper bound on the number of sentences requested.
    timeout : int
        Seconds to wait for the model.

    Returns
    -------
    list[Proposal]
        Proposals with a known category; malformed replies yield an empty list.
    """
    prompt = (
        f"Choose at most {max_quotes} sentences to highlight in this page. "
        f"Categories: {', '.join(sorted(CATEGORY_COLORS))}.\n\n"
        f"{text[:_MAX_PAGE_CHARS]}"
    )
    try:
        raw = json.loads(
            chat(prompt, provider=provider, model=model, host=host, timeout=timeout)
        )
    except (json.JSONDecodeError, KeyError):
        return []
    proposals = [
        Proposal(quote=item["quote"], category=item["category"])
        for item in raw.get("highlights", [])
        if item.get("category") in CATEGORY_COLORS and item.get("quote")
    ]
    return proposals[:max_quotes]


def annotate_page(
    page: Any,  # noqa: ANN401 - pymupdf.Page; the package ships no stubs
    proposals: Iterable[Proposal],
    *,
    author: str,
) -> PageResult:
    """Highlight every proposal that can be located in *page*.

    Parameters
    ----------
    page : Any
        A ``pymupdf.Page``; typed loosely because pymupdf ships no stubs.
    proposals : Iterable[Proposal]
        Candidate sentences.
    author : str
        Name recorded as the annotation author.

    Returns
    -------
    PageResult
        What was highlighted, and what was dropped for not being on the page.
    """
    applied: list[Proposal] = []
    dropped: list[Proposal] = []
    for proposal in proposals:
        quads = locate(page, proposal.quote)
        if not quads:
            dropped.append(proposal)
            continue
        annot = page.add_highlight_annot(quads)
        annot.set_colors(stroke=CATEGORY_COLORS[proposal.category])
        annot.set_info(title=author, content=proposal.category)
        annot.update()
        applied.append(proposal)
    return PageResult(
        number=page.number + 1,
        applied=tuple(applied),
        dropped=tuple(dropped),
    )


def parse_pages(spec: str | None, count: int) -> Sequence[int]:
    """Turn a ``1-8`` style range into 0-based page indices.

    Parameters
    ----------
    spec : str | None
        Inclusive 1-based range, a single page, or None for every page.
    count : int
        Number of pages in the document.

    Returns
    -------
    Sequence[int]
        0-based indices, clipped to the document.

    Raises
    ------
    ValueError
        If *spec* is not a page or a page range.
    """
    if spec is None:
        return range(count)
    try:
        first, _, last = spec.partition("-")
        start = int(first)
        end = int(last) if last else start
    except ValueError:
        msg = f"page range must look like '3' or '1-8', got {spec!r}"
        raise ValueError(msg) from None
    return range(max(start - 1, 0), min(end, count))


@click.command()
@click.argument(
    "pdf",
    type=click.Path(exists=True, dir_okay=False, path_type=Path),
)
@click.option(
    "--provider",
    type=click.Choice(PROVIDERS),
    default="ollama",
    show_default=True,
    help="Where inference runs. gemini bills per token; ollama is local.",
)
@click.option("--model", default=None, help="Model name; provider default if unset.")
@click.option("--host", default=None, help="Base URL; provider default if unset.")
@click.option(
    "--max-quotes",
    default=3,
    show_default=True,
    help="Maximum highlights per page.",
)
@click.option("--pages", default=None, help="Page range, 1-based, e.g. '1-8'.")
@click.option(
    "--author", default="ollama", show_default=True, help="Annotation author."
)
@click.option(
    "--in-place",
    is_flag=True,
    help="Write into PDF rather than a copy (Zotero will re-sync the file).",
)
@click.option(
    "--timeout",
    default=_TIMEOUT_S,
    show_default=True,
    help="Seconds to wait per page.",
)
@click.option("--dry-run", is_flag=True, help="Report proposals, write nothing.")
def main(  # noqa: PLR0913
    pdf: Path,
    *,
    provider: str,
    model: str | None,
    host: str | None,
    max_quotes: int,
    pages: str | None,
    author: str,
    in_place: bool,
    timeout: int,
    dry_run: bool,
) -> None:
    """Highlight key sentences in PDF using a local or hosted model."""
    model = model or DEFAULT_MODELS[provider]
    host = host or DEFAULT_HOSTS[provider]
    doc = pymupdf.open(pdf)
    results: list[PageResult] = []
    for index in parse_pages(pages, doc.page_count):
        page = doc[index]
        text = page.get_text()
        if not text.strip():
            continue
        proposals = propose(
            text,
            provider=provider,
            model=model,
            host=host,
            max_quotes=max_quotes,
            timeout=timeout,
        )
        result = annotate_page(page, proposals, author=author)
        results.append(result)
        for proposal in result.applied:
            click.echo(f"p{result.number} {proposal.category:12} {proposal.quote[:60]}")
        for proposal in result.dropped:
            click.echo(
                f"p{result.number} {'dropped':12} {proposal.quote[:60]}",
                err=True,
            )

    applied = sum(len(r.applied) for r in results)
    dropped = sum(len(r.dropped) for r in results)
    click.echo(f"{applied} highlighted, {dropped} dropped (not found on the page)")

    if dry_run:
        click.echo("dry run: nothing written")
    elif in_place:
        doc.save(pdf, incremental=True, encryption=pymupdf.PDF_ENCRYPT_KEEP)
        click.echo(f"wrote {pdf}")
    else:
        out = pdf.with_suffix(".annotated.pdf")
        doc.save(out)
        click.echo(f"wrote {out}")
    doc.close()


if __name__ == "__main__":
    main()
