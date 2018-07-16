#!/usr/bin/env bash

case $1 in
  build)
    stack build \
      --test --no-run-tests --bench --no-run-benchmarks \
      --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:avx2 --flag hw-dsv:avx2
    ;;

  test)
    stack test \
      --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:avx2 --flag hw-dsv:avx2
    ;;

  bench)
    stack bench \
      --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:avx2 --flag hw-dsv:avx2
    ;;

  install)
    stack install \
      --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:avx2 --flag hw-dsv:avx2
    ;;

  repl)
    stack repl \
      --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:bmi2 --flag hw-simd:avx2
    ;;
esac
