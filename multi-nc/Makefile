BIN := target/arm-linux-androideabi/release/multi-nc
SRCS := $(wildcard src/*.rs)
TARGET := arm-linux-androideabi

$(BIN) : $(SRCS) Cargo.toml Cargo.lock
	cargo rustc --target $(TARGET) --release

run : $(BIN)
	rs-cross-tool run $^

clean :
	cargo clean
