#!/bin/bash
sbcl --noinform --non-interactive --load text-detection.lisp --end-toplevel-options "$@"
