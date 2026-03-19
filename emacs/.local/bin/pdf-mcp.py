#!/usr/bin/env python3
"""Minimal MCP server for local PDF text extraction via pymupdf.

Implements the Model Context Protocol over STDIO (JSON-RPC 2.0).
Exposes one tool: read_pdf — extracts text with page markers.

Usage (from mcp-hub-servers in Emacs):
  ("pdf" . (:command "python3"
            :args ("/home/dan/workspace/dotfiles/scripts/pdf-mcp.py")))
"""

import json
import sys
from pathlib import Path

try:
    import pymupdf  # pymupdf >= 1.24
except ImportError:
    import fitz as pymupdf  # older API name


_MAX_CHARS = 30000

TOOLS = [
    {
        "name": "read_pdf",
        "description": (
            "Extract text from a local PDF file with page markers. "
            f"Returns up to {_MAX_CHARS} characters. "
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
    }
]


def extract_text(pdf_path: str, max_chars: int = _MAX_CHARS) -> str:
    """Open *pdf_path* with pymupdf and return text with per-page markers.

    Parameters
    ----------
    pdf_path : str
        Absolute path to the PDF file.
    max_chars : int
        Maximum number of characters to return.

    Returns
    -------
    str
        Extracted text with page markers, truncated at *max_chars*.
    """
    pdf_path = str(Path(pdf_path).expanduser().resolve())
    doc = pymupdf.open(pdf_path)
    pages: list[str] = []
    total = 0
    for i, page in enumerate(doc):
        text = page.get_text()
        remaining = max_chars - total
        if len(text) >= remaining:
            pages.extend((
                f"[Page {i + 1}]\n{text[:remaining]}",
                f"\n[Truncated at {max_chars} characters]",
            ))
            break
        pages.append(f"[Page {i + 1}]\n{text}")
        total += len(text)
    header = f"FILE: {Path(pdf_path).name}\nPATH: {pdf_path}\nPAGES: {len(doc)}\n\n"
    return header + "\n".join(pages)


def handle(req: dict) -> dict | None:  # noqa: PLR0911
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
                "serverInfo": {"name": "pdf-mcp", "version": "1.0.0"},
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
                text = extract_text(args["path"])
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
                    "result": {"content": [{"type": "text", "text": text}]},
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
