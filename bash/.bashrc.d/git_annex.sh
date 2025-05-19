# shellcheck shell=bash

# Git Annex Aliases

# Shortcut for 'git-annex'
alias ga='git-annex'
# List all Git Annex repositories
alias lg='git annex list --allrepos'

# Git Annex Functions

# Copy directory structure to a destination
copythistree() {
	find . -type d -exec mkdir -p "$1/{}" \;
}

# Create a directory with the current date and optionally append a description
# $1: Folder in analyses
# $2: Name of the link
# $3: Optional description (e.g., adaptive_22)
lndate() {
	dir=$1/$(date -I)
	if [ -n "$3" ]; then
		dir=${dir}_$3
	fi
	mkdir "$dir"
	ln -s "$dir" "$2"
}

# Initialize a Git repository with Git Annex
# $1: Annex configuration parameter
ga_init() {
	git init
	git annex init "$1"
	touch README
	git add .
	git commit -m 'Initialization'
}

# Find Git Annex duplicate keys
ga_dup() {
	# shellcheck disable=SC2016
	git annex find --include '*' --format='${escaped_key} :${file}\n' |
		cut -d '-' -f 4- | sort | uniq --all-repeated=separate -w 63
}

# Remove Git Annex duplicate keys from the repository
ga_duprm() {
	ga-dup | awk -vRS= -vFS='\n' '{for (i = 2; i <= NF; i++) print $i}' |
		cut -d ':' -f 2- | xargs -d '\n' git rm
}

# Remove Git Annex duplicate keys from the repository and stage the removal
ga_duprmgit() {
	ga-duprm
	git add -u
}
