#!/bin/bash
set -eu

MODEL_DIR=face_detector_models
mkdir -p $MODEL_DIR
pushd $MODEL_DIR
curl -O "https://raw.githubusercontent.com/opencv/opencv/master/samples/dnn/face_detector/deploy.prototxt"
curl -O "https://raw.githubusercontent.com/opencv/opencv_3rdparty/dnn_samples_face_detector_20180205_fp16/res10_300x300_ssd_iter_140000_fp16.caffemodel"
curl -O "https://raw.githubusercontent.com/pyannote/pyannote-data/master/openface.nn4.small2.v1.t7"
popd
