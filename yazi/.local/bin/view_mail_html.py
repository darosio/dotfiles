#!/usr/bin/env python
"""Provide functionality to extract text from MIME email messages."""

from __future__ import annotations

import sys
from email import policy
from email.parser import BytesParser
from pathlib import Path
from typing import TYPE_CHECKING

import html2text

if TYPE_CHECKING:
    from email.message import Message


def _decode_payload(part: Message) -> str:
    """Safely decode the payload of a message part to a string."""
    payload = part.get_payload(decode=True)
    if not isinstance(payload, bytes):
        return ""

    charset = part.get_content_charset() or "utf-8"
    try:
        return payload.decode(charset, errors="replace")
    except (UnicodeDecodeError, LookupError):
        # Fallback to utf-8 if the declared charset is invalid
        return payload.decode("utf-8", errors="replace")


def extract_text_mime(file_path: str) -> str:
    """Extract and return the text content from a MIME email message.

    Parameters
    ----------
    file_path : str
        Path to the email message file.

    Returns
    -------
    str
        Extracted text content from the email. Returns an empty string if no
        text content is found.
    """
    path = Path(file_path)
    with path.open("rb") as file:
        msg = BytesParser(policy=policy.default).parse(file)

    # Display main email details
    print(f"From: {msg.get('From', 'N/A')}")
    print(f"To: {msg.get('To', 'N/A')}")
    print(f"Subject: {msg.get('Subject', 'N/A')}")
    print("-" * 50)

    # If the email is not multipart, process it directly
    if not msg.is_multipart():
        content_type = msg.get_content_type()
        if content_type == "text/plain":
            return _decode_payload(msg)
        if content_type == "text/html":
            return html2text.html2text(_decode_payload(msg))
        return ""

    # For multipart emails, prefer plain text over HTML
    text_content = ""
    html_content = ""

    for part in msg.walk():
        content_type = part.get_content_type()
        content_disposition = part.get("Content-Disposition", "")

        # Skip attachments
        if "attachment" in content_disposition:
            print(f"[Attachment: {part.get_filename() or 'unknown'}]")
            continue

        if content_type == "text/plain" and not text_content:
            text_content = _decode_payload(part)
        elif content_type == "text/html" and not html_content:
            html_content = html2text.html2text(_decode_payload(part))

    return text_content or html_content


if __name__ == "__main__":
    number_of_arguments = 1 + 1
    if len(sys.argv) < number_of_arguments:
        print(f"Usage: {sys.argv[0]} <path_to_email_file>")
        sys.exit(1)

    content = extract_text_mime(sys.argv[1])
    if content:
        print(content)
