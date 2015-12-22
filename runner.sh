#!/usr/bin/env bash

. runner.sh.conf

CMD=$1

case $CMD in
  receiver.*)
    export IS_RECEIVER=True
    ;;
  sender.*)
    export IS_RECEIVER=False
    ;;
esac

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
  receiver.hs|sender.hs)
    cd hs-graceful-io &&
      stack build &&
      stack exec hs-graceful-io-exe
    ;;
  *)
    echo "Unknown command: $CMD"
    exit 1
    ;;
esac

