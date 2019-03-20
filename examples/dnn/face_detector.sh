#!/bin/bash
sbcl --noinform --non-interactive --load face_detector.lisp --end-toplevel-options "$@"
