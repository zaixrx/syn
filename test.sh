error_out() {
	echo -e "\033[0;31m"
	echo "$1"
	echo -e "\033[0m"
}

success_out() {
	echo -e "\033[0;32m"
	echo "$1"
	echo -e "\033[0m"
}

lookup() {
	for entry in "$1"/*
	do
		if [ -d $entry ]; then
			lookup $entry
		else
			echo "cargo run -- $entry"
			if cargo run -- $entry; then
				success_out "$entry: Succeeded"
			else
				error_out "$entry: Failed"
				exit 69
			fi
		fi
		clear
	done
	success_out "All the tests passed successfully"
}

lookup "./examples"
