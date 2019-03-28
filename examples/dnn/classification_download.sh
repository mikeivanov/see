#!/bin/bash
set -eu
mkdir -p classification_models
pushd classification_models
curl -O https://raw.githubusercontent.com/DeepScale/SqueezeNet/master/SqueezeNet_v1.1/squeezenet_v1.1.caffemodel
curl -o squeezenet_v1.1.deploy.prototxt https://raw.githubusercontent.com/DeepScale/SqueezeNet/master/SqueezeNet_v1.1/deploy.prototxt
curl -O https://raw.githubusercontent.com/opencv/opencv/master/samples/data/dnn/classification_classes_ILSVRC2012.txt
popd
