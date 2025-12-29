#!/usr/bin/env python3
import json
import sys

import requests

SEARXNG_URL = "http://localhost:8080/search"


def search(query, categories=None, language="en"):
    data = {
        "q": query,
        "format": "json",
    }
    if categories:
        data["categories"] = categories
    if language:
        data["language"] = language

    r = requests.post(
        SEARXNG_URL,
        headers={"Content-Type": "application/x-www-form-urlencoded"},
        data=data,
        timeout=30,
    )
    r.raise_for_status()
    return r.json()


def main() -> None:
    for line in sys.stdin:
        req = json.loads(line)

        if req["method"] == "tools/list":
            print(
                json.dumps(
                    {
                        "tools": [
                            {
                                "name": "web_search",
                                "description": "Search the web using SearXNG",
                                "input_schema": {
                                    "type": "object",
                                    "properties": {
                                        "query": {"type": "string"},
                                        "categories": {"type": "string"},
                                    },
                                    "required": ["query"],
                                },
                            }
                        ]
                    }
                ),
                flush=True,
            )

        elif req["method"] == "tools/call":
            args = req["params"]["arguments"]
            results = search(
                args["query"],
                categories=args.get("categories"),
            )

            print(
                json.dumps(
                    {
                        "content": [
                            {"type": "text", "text": json.dumps(results, indent=2)}
                        ]
                    }
                ),
                flush=True,
            )


if __name__ == "__main__":
    main()
