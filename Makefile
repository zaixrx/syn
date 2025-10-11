all:
	cargo build --release
	mv target/debug/syn syn.out
	echo "created ./syn.out"
