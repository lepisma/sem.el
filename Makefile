release: sem-core.so

sem-core.so: ./target/release/libsem_el.so
	cp ./target/release/libsem_el.so sem-core.so

./target/release/libsem_el.so: ./src/lib.rs
	cargo build --release
