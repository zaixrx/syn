search_dir=./syn
for entry in "$search_dir"/*
do
	echo "======"
	echo "$entry"
 	if [ -d $entry ]; then
 		# run_test $entry
		echo "directory $entry"
 	else
 		if cargo run -- $entry; then
			echo "SUCCESS"
		else
			echo "FAILURE"
			exit
 		fi
 	fi
done

