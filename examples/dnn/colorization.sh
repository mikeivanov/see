#!/bin/bash
sbcl --noinform --non-interactive --load colorization.lisp --end-toplevel-options "$@"
