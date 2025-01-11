all: sem-core.so

sem-core.so: ./target/debug/libsem_el.so
	cp ./target/debug/libsem_el.so sem-core.so

./target/debug/libsem_el.so: ./src/lib.rs
	cargo build
