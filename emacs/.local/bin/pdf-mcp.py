#!/usr/bin/env python3
"""MCP server for local PDF text extraction with Zotero citation lookup.

Implements the Model Context Protocol over STDIO (JSON-RPC 2.0).
Exposes tools:
  - read_pdf:       Extract text with page markers; DOI in header if found
  - extract_doi:    Extract DOI from PDF (first 3 pages only)
  - zotero_lookup:  Check if a DOI/filename is in the Zotero BBT auto-export;
                    returns the citation key or signals that manual import is needed

Usage (from mcp-hub-servers in Emacs):
  ("pdf" . (:command "uv"
            :args ("run" "--with" "pymupdf"
                   "/home/dan/.local/bin/pdf-mcp.py")
            :env (:ZOTERO_BIB_FILE "~/Sync/biblio/main.bib")))
"""

import json
import os
import re
import sys
from pathlib import Path

try:
    import pymupdf  # pymupdf >= 1.24
except ImportError:
    import fitz as pymupdf  # older API name

_MAX_CHARS = 30000
_DOI_SEARCH_PAGES = 3

# DOI regex pattern (simplified but effective)
DOI_PATTERN = re.compile(r'(https?://doi\.org/)?(10\.\d{4,}/[^\s"<>]+)', re.IGNORECASE)


def extract_doi_from_text(text: str) -> str | None:
    """Extract the first DOI from text.

    Parameters
    ----------
    text : str
        Text content to search for DOI.

    Returns
    -------
    str | None
        The DOI string (e.g., "10.1103/PhysRevD.99.123456") or None.
    """
    match = DOI_PATTERN.search(text)
    if match:
        doi = match.group(2)
        # Clean up trailing punctuation
        return re.sub(r"[.,;]+$", "", doi)
    return None


def extract_text_with_metadata(pdf_path: str, max_chars: int = _MAX_CHARS) -> dict:
    """Open *pdf_path* and return text with metadata including DOI.

    Parameters
    ----------
    pdf_path : str
        Absolute path to the PDF file.
    max_chars : int
        Maximum number of characters to return.

    Returns
    -------
    dict
        Dictionary with 'text', 'doi', 'pages', 'filename', 'path'.
    """
    pdf_path = str(Path(pdf_path).expanduser().resolve())
    doc = pymupdf.open(pdf_path)
    pages: list[str] = []
    total = 0
    doi = None
    # First 3 pages for DOI extraction (usually in header)
    doi_search_text = ""

    for i, page in enumerate(doc):
        text = page.get_text()
        if i < _DOI_SEARCH_PAGES:
            doi_search_text += text + "\n"
        remaining = max_chars - total
        if len(text) >= remaining:
            pages.extend((
                f"[Page {i + 1}]\n{text[:remaining]}",
                f"\n[Truncated at {max_chars} characters]",
            ))
            break
        pages.append(f"[Page {i + 1}]\n{text}")
        total += len(text)

    # Extract DOI from first few pages
    doi = extract_doi_from_text(doi_search_text)

    header = f"FILE: {Path(pdf_path).name}\nPATH: {pdf_path}\nPAGES: {len(doc)}"
    if doi:
        header += f"\nDOI: {doi}"
    header += "\n\n"

    return {
        "filename": Path(pdf_path).name,
        "path": pdf_path,
        "pages": len(doc),
        "doi": doi,
        "text": header + "\n".join(pages),
    }


def extract_text(pdf_path: str, max_chars: int = _MAX_CHARS) -> str:
    """Extract text only for backward compatibility.

    Parameters
    ----------
    pdf_path : str
        Absolute path to the PDF file.
    max_chars : int
        Maximum number of characters to return.

    Returns
    -------
    str
        Extracted text with metadata header.
    """
    result = extract_text_with_metadata(pdf_path, max_chars)
    return result["text"]


def extract_doi(pdf_path: str) -> dict:
    """Extract DOI from PDF file.

    Parameters
    ----------
    pdf_path : str
        Absolute path to the PDF file.

    Returns
    -------
    dict
        Result with 'doi' key (may be None) and 'path'.
    """
    pdf_path = str(Path(pdf_path).expanduser().resolve())
    # Extract text from first 3 pages only
    doc = pymupdf.open(pdf_path)
    text = ""
    for i, page in enumerate(doc):
        if i >= _DOI_SEARCH_PAGES:
            break
        text += page.get_text() + "\n"

    doi = extract_doi_from_text(text)
    return {
        "path": pdf_path,
        "filename": Path(pdf_path).name,
        "doi": doi,
    }


def zotero_lookup(
    doi: str | None = None, filename: str | None = None, bib_path: Path | None = None
) -> dict:
    """Find a citation key and BibTeX entry in the Zotero Better BibTeX auto-export.

    Reads the .bib file configured via the ZOTERO_BIB_FILE environment variable
    (default: ~/Sync/biblio/main.bib).  Searches by DOI first, then by PDF filename.

    Parameters
    ----------
    doi : str | None
        DOI string to search for, e.g. "10.1038/s41586-021-03819-2".
    filename : str | None
        PDF filename (basename only) to match against BibTeX file fields.
    bib_path : Path | None
        Override the .bib file path (used in tests; takes precedence over env var).

    Returns
    -------
    dict
        On success: {"found": True, "key": "Smith2023", "bibtex": "@article{...}"}
        On failure: {"found": False, "doi": ..., "filename": ..., "error": ...}
    """
    if bib_path is None:
        bib_path = Path(
            os.environ.get("ZOTERO_BIB_FILE", "~/Sync/biblio/main.bib")
        ).expanduser()
    if not bib_path.exists():
        return {"found": False, "error": f"BibTeX file not found: {bib_path}"}

    content = bib_path.read_text(encoding="utf-8")
    # Split into individual entries on lines that start a new @type{ block
    raw_entries = re.split(r"\n(?=@)", content)

    for raw_entry in raw_entries:
        entry = raw_entry.strip()
        if not entry.startswith("@"):
            continue
        key_match = re.match(r"@\w+\{([^,]+),", entry)
        if not key_match:
            continue
        key = key_match.group(1).strip()

        if doi:
            doi_match = re.search(r"doi\s*=\s*\{([^}]+)\}", entry, re.IGNORECASE)
            if doi_match:
                stored = doi_match.group(1).strip().lower()
                needle = doi.strip().lower()
                # Accept substring match to handle https://doi.org/ prefixes
                if needle in stored or stored in needle:
                    return {"found": True, "key": key, "bibtex": entry}

        if filename:
            file_match = re.search(r"file\s*=\s*\{([^}]+)\}", entry, re.IGNORECASE)
            if (
                file_match
                and Path(filename).name.lower() in file_match.group(1).lower()
            ):
                return {"found": True, "key": key, "bibtex": entry}

    return {"found": False, "doi": doi, "filename": filename}


TOOLS = [
    {
        "name": "read_pdf",
        "description": (
            "Extract text from a local PDF file with page markers. "
            f"Returns up to {_MAX_CHARS} characters. "
            "Also extracts DOI from the first few pages if present. "
            "Accepts absolute paths or paths starting with ~ (home directory)."
        ),
        "inputSchema": {
            "type": "object",
            "properties": {
                "path": {
                    "type": "string",
                    "description": "Absolute path to the PDF file",
                }
            },
            "required": ["path"],
        },
    },
    {
        "name": "extract_doi",
        "description": (
            "Extract DOI from a PDF file. "
            "Searches the first few pages for DOI patterns. "
            "Returns the DOI string or null if not found."
        ),
        "inputSchema": {
            "type": "object",
            "properties": {
                "path": {
                    "type": "string",
                    "description": "Absolute path to the PDF file",
                }
            },
            "required": ["path"],
        },
    },
    {
        "name": "zotero_lookup",
        "description": (
            "Check if a paper is in your Zotero library via the Better BibTeX "
            "auto-exported .bib file. Search by DOI (preferred) or PDF filename. "
            "If found, returns the citation key for use as [cite:@Key] in org-cite. "
            "If not found, report the DOI so the user can add it to Zotero manually."
        ),
        "inputSchema": {
            "type": "object",
            "properties": {
                "doi": {
                    "type": "string",
                    "description": "DOI string, e.g. '10.1038/s41586-021-03819-2'",
                },
                "filename": {
                    "type": "string",
                    "description": (
                        "PDF filename (basename) to match against Zotero file fields"
                    ),
                },
            },
        },
    },
]


def handle(  # noqa: C901, PLR0911, PLR0912
    req: dict,
) -> dict | None:
    """Dispatch a JSON-RPC request and return the response dict (or None).

    Parameters
    ----------
    req : dict
        Parsed JSON-RPC 2.0 request object.

    Returns
    -------
    dict | None
        JSON-RPC response, or None for notifications.
    """
    method = req.get("method", "")
    req_id = req.get("id")

    if method == "initialize":
        return {
            "jsonrpc": "2.0",
            "id": req_id,
            "result": {
                "protocolVersion": "2025-03-26",
                "capabilities": {"tools": {}},
                "serverInfo": {"name": "pdf-mcp", "version": "2.0.0"},
            },
        }

    if method == "initialized":
        return None  # notification — no response

    if method == "tools/list":
        return {"jsonrpc": "2.0", "id": req_id, "result": {"tools": TOOLS}}

    if method == "tools/call":
        tool = req["params"].get("name")
        args = req["params"].get("arguments", {})

        if tool == "read_pdf":
            try:
                result = extract_text_with_metadata(args["path"])
                response_text = result["text"]
            except Exception as exc:  # noqa: BLE001
                return {
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "result": {
                        "content": [{"type": "text", "text": f"Error: {exc}"}],
                        "isError": True,
                    },
                }
            else:
                return {
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "result": {"content": [{"type": "text", "text": response_text}]},
                }

        if tool == "zotero_lookup":
            try:
                result = zotero_lookup(
                    doi=args.get("doi"),
                    filename=args.get("filename"),
                )
                if result["found"]:
                    response_text = (
                        f"Citation key: {result['key']}\n\n{result['bibtex']}"
                    )
                else:
                    response_text = (
                        "Not found in Zotero library."
                        + (f" DOI: {result.get('doi')}" if result.get("doi") else "")
                        + (
                            f" File: {result.get('filename')}"
                            if result.get("filename")
                            else ""
                        )
                        + (f"\n{result['error']}" if result.get("error") else "")
                    )
            except Exception as exc:  # noqa: BLE001
                return {
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "result": {
                        "content": [{"type": "text", "text": f"Error: {exc}"}],
                        "isError": True,
                    },
                }
            else:
                return {
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "result": {"content": [{"type": "text", "text": response_text}]},
                }

        if tool == "extract_doi":
            try:
                result = extract_doi(args["path"])
                if result["doi"]:
                    response_text = f"DOI: {result['doi']}\nFile: {result['filename']}"
                else:
                    response_text = f"No DOI found in: {result['filename']}"
            except Exception as exc:  # noqa: BLE001
                return {
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "result": {
                        "content": [{"type": "text", "text": f"Error: {exc}"}],
                        "isError": True,
                    },
                }
            else:
                return {
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "result": {"content": [{"type": "text", "text": response_text}]},
                }

    if req_id is not None:
        return {
            "jsonrpc": "2.0",
            "id": req_id,
            "error": {"code": -32601, "message": "Method not found"},
        }
    return None


def main() -> None:
    """Read JSON-RPC requests from stdin, write responses to stdout."""
    for raw in sys.stdin:
        stripped = raw.strip()
        if not stripped:
            continue
        try:
            req = json.loads(stripped)
            resp = handle(req)
            if resp is not None:
                print(json.dumps(resp), flush=True)
        except Exception as exc:  # noqa: BLE001
            print(
                json.dumps({
                    "jsonrpc": "2.0",
                    "id": None,
                    "error": {"code": -32700, "message": str(exc)},
                }),
                flush=True,
            )


if __name__ == "__main__":
    main()
