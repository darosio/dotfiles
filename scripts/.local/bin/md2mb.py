#!/usr/bin/env python
#
# Frédéric Grosshans, 19 January 2012
# Nathan R. Yergler, 6 June 2010. https://yergler.net/2010/06/06/batteries-included-or-maildir-to-mbox-again/
# This file does not contain sufficient creative expression to invoke assertion of
# copyright. No warranty is expressed or implied; use at your own risk.
"""Convert maildir to mbox.

Uses Python's included mailbox library to convert mail archives from maildir
[http://en.wikipedia.org/wiki/Maildir] to mbox
[http://en.wikipedia.org/wiki/Mbox] format, including subfolders.

See http://docs.python.org/library/mailbox.html#mailbox.Mailbox for
full documentation on this library.

---

To run, save as md2mb.py and run:

$ python md2mb.py [maildir_path] [mbox_filename]

[maildir_path] should be the the path to the actual maildir (containing new,
cur, tmp, and the subfolders, which are hidden directories with names like
.subfolde.subsubfolder.subsubsbfolder);

[mbox_filename] will be newly created, as well as a [mbox_filename].sbd the
directory.
"""
from __future__ import annotations

import email
import mailbox
import sys
from io import BytesIO
from pathlib import Path


def custom_message_from_file(file: BytesIO) -> email.message.Message:
    """Convert file content to email message."""
    try:
        content = file.read().decode("utf-8")
    except UnicodeDecodeError:
        content = file.read().decode("latin1")
    return email.message_from_string(content)


class UTF8Mbox(mailbox.mbox):
    """Subclass of mbox that uses utf-8 for message encoding."""

    def _dump_message(
        self,
        message: email.message.Message,
        target: BytesIO,
        mangle_from_: bool,  # noqa: FBT001,ARG002 "Need to override mailbox.UTF8Mbox"
    ) -> None:
        """Write message to file from mbox mailbox."""
        # Convert message to string
        msg_str = message.as_string()
        # Encode the message to bytes
        msg_bytes = msg_str.encode("utf-8")
        # Write bytes to target mbox file
        target.write(msg_bytes + b"\n")


def maildir2mailbox(maildirname: str, mboxfilename: str) -> None:
    """Convert maildir to mbox format."""
    # open the existing maildir and the target mbox file
    maildir = mailbox.Maildir(maildirname, custom_message_from_file)
    mbox = UTF8Mbox(mboxfilename)
    # lock the mbox
    mbox.lock()
    # iterate over messages in the maildir and add to the mbox
    for msg in maildir:
        mbox.add(msg)
    # close and unlock
    mbox.close()
    maildir.close()


# Creates the main mailbox
dirname = Path(sys.argv[-2])
mboxname = Path(sys.argv[-1])
print(f"{dirname} -> {mboxname}")
mboxdirname = mboxname.with_suffix(".sbd")
maildir2mailbox(dirname, mboxname)
if not mboxdirname.exists():
    mboxdirname.mkdir(parents=True)

listofdirs = [dn for dn in dirname.iterdir() if dn.name not in ["new", "cur", "tmp"]]

for curfold in listofdirs:
    curlist = [mboxname, *curfold.split(".")]
    curpath = Path("/").joinpath(*[dn + ".sbd" for dn in curlist if dn])
    curpath_obj = Path(curpath)  # convert string path to Path
    if not curpath_obj.exists():
        curpath_obj.mkdir(parents=True, exist_ok=True)
    print("| " + curfold + " -> " + curpath[:-4])
    maildir2mailbox(dirname / curfold, curpath[:-4])

print("Done")
