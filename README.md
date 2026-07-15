# Yphtach Lelkes Website

Personal academic website built with Hugo.

## Local Development

```sh
python3 scripts/build_publications.py
PATH=/opt/homebrew/bin:$PATH hugo server --disableFastRender
```

## Production Build

```sh
python3 scripts/build_publications.py
PATH=/opt/homebrew/bin:$PATH hugo --gc --minify
```
