#!/usr/bin/env bash

docker run --net=host -it -v $PWD:/root/proj --entrypoint "/bin/bash" sakshamsharma/docker-hakyll:v2
