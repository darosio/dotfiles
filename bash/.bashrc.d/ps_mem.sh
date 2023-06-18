# shellcheck shell=bash
alias memory='sudo ps_mem -p $(pgrep -d, -u $USER)' # pipx install ps_mem
memory_users () {
	for i in $(ps -e -o user= | sort | uniq); do
		printf '%-20s%10s\n' "$i" "$(sudo ps_mem --total -p "$(pgrep -d, -u "$i")")"
	done
}
