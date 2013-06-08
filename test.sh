#!/bin/sh

echo a

(
  echo b
  sleep 3
  echo c
) &

echo d
