#!/bin/sh

rm -rf dist
git clone --single-branch -b gh-pages git@github.com:triplepointfive/opendrod.git dist
elm make src/Main.elm --output=main.js --optimize
mv main.js dist
cp -r src/assets dist
cd dist
git add .
git commit -m "Build"
git push origin gh-pages
