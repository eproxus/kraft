#!/bin/sh
set -eu

cd priv/styles

yarn install --silent
npx tailwindcss build kraft.css -o ../web/static/assets/styles/kraft.css
