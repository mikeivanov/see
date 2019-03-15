#!/bin/bash
sbcl --noinform --non-interactive --load fast_neural_style.lisp --end-toplevel-options "$@"
