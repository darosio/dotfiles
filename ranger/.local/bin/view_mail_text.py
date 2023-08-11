#!/usr/bin/env python3
"""Provide functionality to extract text from MIME email messages."""

from __future__ import annotations

import sys
from email import policy
from email.parser import BytesParser
from pathlib import Path


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
    print(f"From: {msg['From']}")
    print(f"To: {msg['To']}")
    print(f"Subject: {msg['Subject']}")
    print("-" * 50)

    if not msg.is_multipart():
        # If the message isn't multipart, print it as is if it's plain text
        if msg.get_content_type() == "text/plain":
            return msg.get_payload(decode=True).decode(
                msg.get_content_charset() or "utf-8"
            )
        return ""

    text_content = ""

    # If it is multipart, process each part
    for part in msg.walk():
        # Look for the 'text/plain' part and extract it
        if part.get_content_type() == "text/plain":
            charset = part.get_content_charset() or "utf-8"
            try:
                text_content = part.get_payload(decode=True).decode(
                    charset, errors="replace"
                )
            except UnicodeDecodeError:
                text_content = part.get_payload(decode=True).decode(
                    "utf-8", errors="replace"
                )

        # Indicate attachments
        content_disposition = part.get("Content-Disposition")
        if content_disposition and "attachment" in content_disposition:
            print(f"[Attachment: {part.get_filename()}]")

    return text_content


if __name__ == "__main__":
    content = extract_text_mime(sys.argv[1])
    if content:
        print(content)
