#!/usr/bin/env python3

"""Add notmuch tags to maildir w/ X-Keywords header in mails."""
from __future__ import annotations

import configparser
import logging
import os
import sys
from pathlib import Path

import notmuch


def get_notmuch_config() -> str:
    """Get notmuch config based on precedence."""
    config_path = Path(sys.argv[1]) if len(sys.argv) > 1 else None
    if config_path and (config_path / ".notmuch").is_dir():
        logging.debug("Using notmuch config from command line")
        notmuch_config_fname = config_path
    elif "NOTMUCH_CONFIG" in os.environ:
        logging.debug("Using notmuch config from env variable")
        notmuch_config_fname = Path(os.environ["NOTMUCH_CONFIG"])
    else:
        notmuch_config_fname = Path.home() / ".notmuch-config"
    notmuch_config = configparser.ConfigParser()
    notmuch_config.read(notmuch_config_fname)
    try:
        logging.debug("Using notmuch config from config file")
        return notmuch_config.get("database", "path")
    except configparser.NoOptionError as err:
        logging.exception("No usable notmuch config")
        error_msg = "No notmuch config found"
        raise RuntimeError(error_msg) from err


def add_tags(
    notmuch_db: notmuch.Database, add_gmail: bool = False  # noqa: FBT001,FBT002
) -> None:
    """Add tags from X-Keywords header to notmuch."""
    all_messages = notmuch.Query(notmuch_db, "").search_messages()
    for message in all_messages:
        try:
            message_tags = message.get_header("X-Keywords")
            if message_tags:
                for tag in message_tags.split(","):
                    # remove whitespace
                    tag_str = tag.strip()  # renamed to avoid overwritten loop variable
                    if tag_str.startswith("\\") and add_gmail:
                        message.add_tag(
                            tag_str[1:]
                        )  # remove the leading '\\' for gmail tags
                    else:
                        logging.debug(
                            "Adding tag %s to msg <%s>",
                            tag_str,
                            message.get_message_id(),
                        )
                        message.add_tag(tag_str)
        except notmuch.errors.NullPointerError:
            # This will handle the exception if the header doesn't exist for a message.
            pass


def main(add_gmail: bool = False) -> None:  # noqa: FBT001,FBT002
    """Run the script."""
    notmuch_config = get_notmuch_config()
    logging.info("Notmuch config used: %s", notmuch_config)
    logging.info("Setting up notmuch database")
    notmuch_database = notmuch.Database(
        path=notmuch_config, create=False, mode=notmuch.Database.MODE.READ_WRITE
    )
    add_tags(notmuch_database, add_gmail)


if __name__ == "__main__":
    if len(sys.argv) == 1 or sys.argv[1] in ("-h", "--help"):
        print(
            f"""Usage: {sys.argv[0]} [config_path] [--add-gmail-tags]
            This script will try config filenames in the order:
            1. [config_path] from command line
            2. NOTMUCH_CONFIG environment variable
            3. ~/.notmuch-config
            Set LOGLEVEL env variable for logging (e.g., ERROR, INFO, DEBUG)"""
        )

    LOG_FNAME = Path.home() / "addtags.log"
    logging.basicConfig(
        filename=LOG_FNAME, level=os.environ.get("LOGLEVEL", logging.ERROR)
    )
    ADD_GMAIL = "--add-gmail-tags" in sys.argv
    main(add_gmail=ADD_GMAIL)
