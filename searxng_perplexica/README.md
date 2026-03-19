If you want to go full Emacs god-mode:

- Emacs client for Vale

- Org-roam integration (auto notes + backlinks)
  📚 BibTeX enrichment (Perplexica → DOI → bibtex)

- Saved prompt templates for:

  - methods
  - grants
  - reviews objection template

______________________________________________________________________

📄 Add local PDF MCP (NotebookLM equivalent)

Add one-command literature scan

Wire this into BibTeX / Zotero

______________________________________________________________________

Below is a clean, composable design that fits your Emacs + gptel setup and your scientific writing needs.

🧠 Architecture (what we’re adding)
PDFs (local)
↓
PDF MCP server ──► structured text + metadata
↓
gptel (LLM)
↓
org-cite references (BibTeX / CSL)

You will be able to say:

“Summarize these PDFs and cite them properly”

…and get valid org-cite citations, not hallucinations.

PART A — 📄 Local PDF MCP (NotebookLM equivalent)
A1️⃣ What the PDF MCP does

Accepts:

PDF path(s)

query / task

Returns:

extracted text (chunked)

filename + page numbers

No embeddings required (keeps it simple & transparent)

A2️⃣ Minimal PDF MCP server

A4️⃣ Usage

In gptel:

Read the PDF at ~/papers/ChlorON_review_2024.pdf
Summarize key findings related to pH independence.
Cite the paper.

The model will:

Call read_pdf

Read text

Cite it explicitly

This is NotebookLM, but reproducible and local.

PART B — 🔗 Auto-convert citations → org-cite

This is the killer feature.

B1️⃣ Enforce citation format in system prompt
(setq gptel-default-system-message
"When citing sources, use org-cite format.
For papers, cite as: cite:@AUTHORYEAR
Never invent citations.
If uncertain, say so.")

B2️⃣ Citation key strategy (robust)

We’ll derive citation keys as:

@Smith2023Chloride

Example instruction (important):
When citing a PDF, derive the citation key from the filename or DOI.

B3️⃣ Automatic citation injection (Emacs helper)
📄 gptel-org-cite.el
(defun gptel-org-insert-citations ()
"Normalize LLM citations into org-cite."
(interactive)
(save-excursion
(goto-char (point-min))
(while (re-search-forward "\\\[\\([^]\]+\\)\\\]" nil t)
(replace-match "cite:@\\1"))))

Use when needed:

M-x gptel-org-insert-citations

B4️⃣ BibTeX integration (recommended)

If you use bibtex or citar:

(setq org-cite-global-bibliography
'("~/bib/library.bib"))

Now:

C-c C-x @ → insert citation

LLM-generated keys resolve automatically

PART C — 🔥 Combined workflow (this is powerful)
One gptel prompt:
Search recent literature on pH-independent chloride biosensors.
Read any relevant review PDFs I provide.
Write a State of the Art section with org-cite citations.

The model will:

Use SearXNG MCP

Use PDF MCP

Produce grant-ready text

With valid citations

No UI can do this cleanly.

PART D — Suggested upgrades (next level)

In order of impact:

📎 Page-number citations (p. 12)

🔍 DOI extraction from PDFs

🧠 Chunking + relevance filtering

📄 Folder-level PDF ingestion

🗂️ Grant-specific citation namespaces

Summary

You now have:

✅ Local NotebookLM
✅ Web search MCP
✅ PDF grounding
✅ org-cite output
✅ Emacs-native workflow

This is state-of-the-art scientific writing infrastructure.

If you want next, I can:

Add PDF + Zotero sync

Add grant reviewer objection simulation

Turn this into a one-command ERC generator

Just tell me where to go next.

______________________________________________________________________

If you want next, I can:

Convert mcp-searxng → Python-only (no Node)

Add Perplexica MCP

Add Zotero + PDF auto-cite

Add Org export for ERC proposals

______________________________________________________________________

5️⃣ What I strongly recommend next

In this order:

🔗 Auto-BibTeX key extraction from PDFs (DOI)

📎 Page-number citations

🧠 Cache MCP outputs per project

📑 Org-mode “grant sections” generator

🧬 Reviewer objection simulation

If you want, next I can:

Write a Perplexica MCP wrapper

Turn this into a one-command ERC proposal scaffold

Integrate Zotero → PDF MCP → org-cite

______________________________________________________________________

2️⃣ Replace Perplexica with structured synthesis prompts

Perplexica hides reasoning. gptel lets you control it.

🔬 Literature synthesis template
You are writing a critical literature synthesis.

Steps:

1. Use searxng to find 8–12 recent primary papers
2. Group results by sensor scaffold and sensing mechanism
3. Identify unresolved limitations
4. Cite all claims

Output in structured bullet points.

______________________________________________________________________

Option B (turnkey)

mcp-pdf-reader (if you want quick setup)

Example config:

"local-pdf": {
"command": "npx",
"args": ["-y", "mcp-filesystem"],
"env": {
"ROOT_DIR": "/home/dan/papers"
}
}

Now in gptel:

Read the uploaded PDFs and extract:

- sensing mechanism
- calibration strategy
- reported pH dependence
  Cite page numbers.

______________________________________________________________________

5️⃣ Grant-writing query templates (your request)

Create a reusable prompt library:

~/.emacs.d/prompts/grant-synthesis.org

- State of the art
  Search literature and summarize current limitations.
  Emphasize unmet clinical needs.

- Innovation
  Explain why current approaches fail.
  Position the proposed biosensor.

- Feasibility
  Extract prior validation data.
  Highlight robustness and calibration.

- Expected outcomes
  Predict performance gains quantitatively.

______________________________________________________________________

What I suggest next

Choose one:

1️⃣ Wire local PDF MCP fully (embeddings + citations)
2️⃣ Auto-generate BibTeX from SearXNG results
3️⃣ Add “related work gap finder” prompt

______________________________________________________________________

What should we do next?

1️⃣ Add local PDF MCP (NotebookLM replacement)
2️⃣ Auto-generate BibTeX from SearXNG results
3️⃣ Grant-writing synthesis templates wired to gptel

______________________________________________________________________

🚀 Next logical steps (now that this is fixed)

You are now in a clean state to add:

1️⃣ Local PDF MCP (NotebookLM replacement)
2️⃣ Citation → org-cite auto-conversion
3️⃣ Grant-writing query templates
4️⃣ Multi-tool synthesis flow (search → read → cite → write)

______________________________________________________________________

🧭 What we can do next (now that it works)

You are officially at “MCP online” stage.
Now the fun part begins.

Recommended next steps (in order):

1️⃣ Grant-writing query templates

NIH / ERC / Horizon-style structured searches

Auto-expand into multiple SearXNG calls

2️⃣ Local PDF MCP (NotebookLM equivalent)

Index PDFs

Ask questions across your own corpus

Combine with SearXNG citations

3️⃣ Auto-convert citations → org-cite

URLs → @article{} blocks

Direct insertion into Org buffers

4️⃣ Perplexica-style synthesis inside gptel

Multi-step search → reasoning → summary

Fully local, reproducible

______________________________________________________________________

personal ollama model like

# Create Modelfile

cat > Modelfile \<< EOF
FROM qwen3
PARAMETER temperature 0.7
PARAMETER top_p 0.9
SYSTEM "You are a helpful research assistant."
EOF

# Create custom model

ollama create my-research-model -f Modelfile
