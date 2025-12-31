#!/usr/bin/env python3
"""
Enhanced Perplexica MCP Server with Scientific Search Features.

This is a prototype extending thetom42/perplexica-mcp with:
1. Scientific search category filtering
2. Grant-writing optimized prompts as MCP resources
3. Emacs/gptel integration helpers
"""

import argparse
import os
from typing import Annotated

import httpx
import uvicorn
from dotenv import load_dotenv
from mcp.server.fastmcp import FastMCP
from pydantic import Field

load_dotenv()

PERPLEXICA_BACKEND_URL = os.getenv(
    "PERPLEXICA_BACKEND_URL", "http://localhost:3000/api/search"
)
PERPLEXICA_READ_TIMEOUT = int(os.getenv("PERPLEXICA_READ_TIMEOUT", 60))

DEFAULT_CHAT_MODEL = None
if os.getenv("PERPLEXICA_CHAT_MODEL_PROVIDER") and os.getenv(
    "PERPLEXICA_CHAT_MODEL_NAME"
):
    DEFAULT_CHAT_MODEL = {
        "provider": os.getenv("PERPLEXICA_CHAT_MODEL_PROVIDER"),
        "name": os.getenv("PERPLEXICA_CHAT_MODEL_NAME"),
    }

DEFAULT_EMBEDDING_MODEL = None
if os.getenv("PERPLEXICA_EMBEDDING_MODEL_PROVIDER") and os.getenv(
    "PERPLEXICA_EMBEDDING_MODEL_NAME"
):
    DEFAULT_EMBEDDING_MODEL = {
        "provider": os.getenv("PERPLEXICA_EMBEDDING_MODEL_PROVIDER"),
        "name": os.getenv("PERPLEXICA_EMBEDDING_MODEL_NAME"),
    }

# Create FastMCP server
mcp = FastMCP(
    "Perplexica-Scientific", dependencies=["httpx", "mcp", "python-dotenv", "uvicorn"]
)


# ============================================================================
# NEW: MCP Resources for Scientific Prompts
# ============================================================================


@mcp.resource("prompts://scientific-synthesis")
def scientific_synthesis_prompt() -> str:
    """System prompt optimized for scientific literature synthesis."""
    return """You are assisting with scientific grant writing and literature review.

SEARCH BEHAVIOR:
- Use the search tool with focus_mode='academicSearch' for scientific queries
- Prefer sources from: PubMed, arXiv, Google Scholar, Semantic Scholar, CrossRef
- For state-of-the-art queries, include "recent advances" or "review" in search

CITATION FORMAT:
- Use org-cite format for citations: [cite:@AuthorYear]
- Include DOI when available: [cite:@Smith2024; doi:10.1234/example]
- Never invent citations - only cite sources from search results

OUTPUT STRUCTURE:
- Use structured sections with clear headings
- Include bullet points for key findings
- End with a "Key References" section listing all cited sources"""


@mcp.resource("prompts://grant-sota")
def grant_sota_prompt() -> str:
    """System prompt for State of the Art grant sections."""
    return """You are writing a "State of the Art" section for a research grant.

SEARCH STRATEGY:
1. Search for: "recent advances" AND "{topic}" AND (review OR overview)
2. Focus on papers from the last 3-5 years
3. Identify key research groups and their contributions

OUTPUT REQUIREMENTS:
- 3-5 paragraphs summarizing the field
- Identify 2-3 major research directions
- Highlight remaining challenges/gaps
- Use [cite:@AuthorYear] format for all claims
- End with transition to your proposed innovation"""


@mcp.resource("prompts://grant-gap-analysis")
def grant_gap_prompt() -> str:
    """System prompt for identifying research gaps."""
    return """You are analyzing gaps in the current literature for a grant proposal.

SEARCH STRATEGY:
1. Search for: "{topic}" AND (limitation OR challenge OR drawback OR unmet)
2. Look for: "future work" OR "remains unclear" OR "poorly understood"

OUTPUT REQUIREMENTS:
- List 3-5 specific limitations of current approaches
- For each limitation, cite the source acknowledging it
- Frame gaps as opportunities for the proposed research
- Use [cite:@AuthorYear] format"""


# ============================================================================
# Enhanced Search Function with Category Support
# ============================================================================


async def perplexica_search(
    query: str,
    focus_mode: str,
    categories: list[str] | None = None,
    chat_model: dict | None = None,
    embedding_model: dict | None = None,
    optimization_mode: str | None = None,
    history: list | None = None,
    system_instructions: str | None = None,
    stream: bool = False,
) -> dict:
    """
    Enhanced search with category filtering support.

    Categories map to SearXNG engine categories:
    - science: pubmed, arxiv, google_scholar, semantic_scholar
    - general: google, bing, duckduckgo
    - news: google_news, bing_news
    - images: google_images, bing_images
    """
    payload = {
        "query": query,
        "focusMode": focus_mode,
        "optimizationMode": optimization_mode or "balanced",
        "history": history or [],
    }

    # NEW: Add categories to search if specified
    if categories:
        # This would need corresponding Perplexica backend support
        # For now, we append category hints to the query
        category_engines = {
            "science": "site:pubmed.ncbi.nlm.nih.gov OR site:arxiv.org OR site:scholar.google.com",
            "news": "type:news",
        }
        category_hints = [
            category_engines.get(c, "") for c in categories if c in category_engines
        ]
        if category_hints:
            payload["query"] = f"{query} ({' OR '.join(filter(None, category_hints))})"

    if chat_model:
        payload["chatModel"] = chat_model
    if embedding_model:
        payload["embeddingModel"] = embedding_model
    if system_instructions:
        payload["systemInstructions"] = system_instructions
    if stream is not None:
        payload["stream"] = stream

    try:
        async with httpx.AsyncClient() as client:
            response = await client.post(
                PERPLEXICA_BACKEND_URL, json=payload, timeout=PERPLEXICA_READ_TIMEOUT
            )
            response.raise_for_status()
            return response.json()
    except httpx.HTTPError as e:
        return {"error": f"HTTP error occurred: {e!s}"}
    except Exception as e:
        return {"error": f"An error occurred: {e!s}"}


@mcp.tool()
async def search(
    query: Annotated[str, Field(description="Search query")],
    focus_mode: Annotated[
        str,
        Field(
            description="Focus mode: webSearch, academicSearch, writingAssistant, wolframAlphaSearch, youtubeSearch, redditSearch"
        ),
    ],
    categories: Annotated[
        list[str] | None,
        Field(
            description="Optional SearXNG categories to filter: science, general, news, images"
        ),
    ] = None,
    chat_model: Annotated[
        dict | None, Field(description="Chat model configuration")
    ] = DEFAULT_CHAT_MODEL,
    embedding_model: Annotated[
        dict | None, Field(description="Embedding model configuration")
    ] = DEFAULT_EMBEDDING_MODEL,
    optimization_mode: Annotated[
        str | None, Field(description="Optimization mode: speed or balanced")
    ] = None,
    history: Annotated[list | None, Field(description="Conversation history")] = None,
    system_instructions: Annotated[
        str | None, Field(description="Custom system instructions")
    ] = None,
    stream: Annotated[bool, Field(description="Whether to stream responses")] = False,
) -> dict:
    """
    Search using Perplexica's AI-powered search engine.

    Enhanced with optional category filtering for scientific workflows.
    Use focus_mode='academicSearch' with categories=['science'] for best
    results on scientific/academic queries.
    """
    if (chat_model or DEFAULT_CHAT_MODEL) is None or (
        embedding_model or DEFAULT_EMBEDDING_MODEL
    ) is None:
        return {
            "error": "Both chatModel and embeddingModel are required. Configure PERPLEXICA_* model env vars or pass them in the request."
        }

    return await perplexica_search(
        query=query,
        focus_mode=focus_mode,
        categories=categories,
        chat_model=chat_model,
        embedding_model=embedding_model,
        optimization_mode=optimization_mode,
        history=history,
        system_instructions=system_instructions,
        stream=stream,
    )


# ============================================================================
# NEW: Scientific Search Convenience Tool
# ============================================================================


@mcp.tool()
async def search_scientific(
    query: Annotated[str, Field(description="Scientific search query")],
    search_type: Annotated[
        str, Field(description="Type: literature, sota (state-of-art), gaps, methods")
    ] = "literature",
    chat_model: Annotated[
        dict | None, Field(description="Chat model configuration")
    ] = DEFAULT_CHAT_MODEL,
    embedding_model: Annotated[
        dict | None, Field(description="Embedding model configuration")
    ] = DEFAULT_EMBEDDING_MODEL,
) -> dict:
    """
    Convenience tool for scientific literature searches.

    Automatically applies appropriate query modifiers based on search_type:
    - literature: General scientific literature search
    - sota: State-of-the-art / recent advances
    - gaps: Research gaps and limitations
    - methods: Methodology and techniques
    """
    query_templates = {
        "literature": f'"{query}" AND (review OR study OR research)',
        "sota": f'"recent advances" AND "{query}" AND (review OR overview)',
        "gaps": f'"{query}" AND (limitation OR challenge OR "future work")',
        "methods": f'"{query}" AND (method OR protocol OR technique OR procedure)',
    }

    enhanced_query = query_templates.get(search_type, query)

    system_instructions = """Focus on peer-reviewed scientific sources.
Cite all claims using [cite:@AuthorYear] format.
Prefer recent publications (last 5 years) unless historical context needed."""

    return await perplexica_search(
        query=enhanced_query,
        focus_mode="academicSearch",
        categories=["science"],
        chat_model=chat_model,
        embedding_model=embedding_model,
        system_instructions=system_instructions,
    )


def main() -> None:
    """Main entry point for the enhanced Perplexica MCP server."""
    parser = argparse.ArgumentParser(description="Perplexica Scientific MCP Server")
    parser.add_argument(
        "transport", choices=["stdio", "sse", "http"], help="Transport type to use"
    )
    parser.add_argument(
        "host",
        nargs="?",
        default="0.0.0.0",
        help="Host to bind to for SSE/HTTP transports (default: 0.0.0.0)",
    )
    parser.add_argument(
        "port",
        nargs="?",
        type=int,
        default=3001,
        help="Port for SSE/HTTP transports (default: 3001)",
    )

    args = parser.parse_args()

    if args.transport == "stdio":
        mcp.run()
    elif args.transport == "sse":
        print(
            f"Starting Perplexica Scientific MCP server (SSE) on {args.host}:{args.port}"
        )
        uvicorn.run(mcp.sse_app(), host=args.host, port=args.port)
    elif args.transport == "http":
        print(
            f"Starting Perplexica Scientific MCP server (HTTP) on {args.host}:{args.port}"
        )
        uvicorn.run(mcp.streamable_http_app(), host=args.host, port=args.port)


if __name__ == "__main__":
    main()
