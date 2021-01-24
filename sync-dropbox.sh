#!/usr/bin/env bash

pushd ~/dropbox-sync
git add .
git commit -m 'from jupiter'
git pull
git push
popd

