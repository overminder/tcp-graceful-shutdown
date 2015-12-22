#!/usr/bin/env bash

. runner.sh.conf

CMD=$1

case $CMD in
  sender.netty)
    cd sender-netty && \
      ./gradlew --daemon run
    ;;
  sender.cf)
    cd sender-objc && \
      make && \
      ./main
    ;;
  receiver.mio)
    cd multi-nc && \
      cargo run --release
    ;;
  *)
    echo "Unknown command: $CMD"
    exit 1
    ;;
esac

