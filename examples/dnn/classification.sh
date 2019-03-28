#!/bin/bash
sbcl --noinform --non-interactive --load classification.lisp --end-toplevel-options "$@"
