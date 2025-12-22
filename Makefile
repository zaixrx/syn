all:
	cargo build --release
	cp target/release/syn syn.out
	echo "created ./syn.out"
