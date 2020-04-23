#! /bin/bash

echo 'none'
./perf/exe/none ./perf/fibonacci.lox > ./perf/none.log
echo 'inlines'
./perf/exe/inlines ./perf/fibonacci.lox > ./perf/inlines.log
echo 'strict-eval-args'
./perf/exe/strict-eval-args ./perf/fibonacci.lox > ./perf/strict-eval-args.log
echo 'full'
./perf/exe/full ./perf/fibonacci.lox > ./perf/full.log
