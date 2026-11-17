#!/usr/bin/env sh
#
# Global agent skills: install the current global set into ~/.agents/skills
# via the skills CLI. Idempotent; re-run on fresh machines.
# Reflects what is actually installed (2026-11-17), not the stale entries in
# ~/.agents/.skill-lock.json — that lock still lists ~30 skills that are not
# on disk (removed or per-project installs); do not install from it.
# Update with: npx -y skills update -g
set -e

npx -y skills add vercel-labs/skills@find-skills -g
npx -y skills add anthropics/knowledge-work-plugins@knowledge-synthesis -g
npx -y skills add anthropics/skills@pdf -g
npx -y skills add K-Dense-AI/scientific-agent-skills@pymc -g
npx -y skills add wshobson/agents@rag-implementation -g
npx -y skills add K-Dense-AI/scientific-agent-skills@statistical-analysis -g
npx -y skills add miqdadbadjuber/anti-slop -g
