#!/usr/bin/env bash

docker pull sakshamsharma/docker-hakyll
contId=$(docker run -d sakshamsharma/docker-hakyll https://github.com/sakshamsharma/acehack)
docker wait ${contId}
docker cp -r ${contId}:/home/hakyll/clone/_site/* _site/
./deploy.sh
