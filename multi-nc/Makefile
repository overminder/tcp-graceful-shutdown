SRCS := $(wildcard src/*.rs)
TARGET := arm-linux-androideabi
BIN := target/$(TARGET)/release/multi-nc

$(BIN) : $(SRCS) Cargo.toml Cargo.lock
	cargo rustc --target $(TARGET) --release

run : $(BIN)
	rs-cross-tool run $^

clean :
	cargo clean
