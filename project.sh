#!/usr/bin/env bash

STACK_FLAGS="
  --flag bits-extra:bmi2
  --flag hw-rankselect-base:bmi2
  --flag hw-rankselect:bmi2
  --flag hw-simd:bmi2
  --flag hw-simd:avx2
  --flag hw-dsv:bmi2
  --flag hw-dsv:avx2
"

cmd="$1"

shift

case "$cmd" in
  build)
    stack build \
      --test --no-run-tests --bench --no-run-benchmarks \
      $STACK_FLAGS "$@"
    ;;

  test)
    stack test \
      $STACK_FLAGS "$@"
    ;;

  bench)
    stack bench \
      $STACK_FLAGS "$@"
    ;;

  repl)
    stack repl \
      $STACK_FLAGS "$@"
    ;;

  install)
    stack install \
      $STACK_FLAGS "$@"
    ;;

  flags)
    echo "Flags: " \
      $STACK_FLAGS "$@"
    ;;
esac
