# Code for nmattia.com

Link: [https://nmattia.com](https://nmattia.com)

## Build

All commands are run from the root of the project, from a terminal:

| Command           | Action                                     |
| :---------------- | :----------------------------------------- |
| `npm ci`          | Install dependencies                       |
| `npm run dev`     | Start local dev server at `localhost:3000` |
| `npm run build`   | Build production site to `./dist/`         |
| `npm run preview` | Preview the build locally                  |
| `npm run format`  | Run formatter                              |
| `npm run upgrade` | Upgrade astro                              |

## Favicon

Additionally, here are the steps for creating the favicons from the SVG (inspired by [this](https://evilmartians.com/chronicles/how-to-favicon-in-2021-six-files-that-fit-most-needs)):

```bash
inkscape ./public/icon.svg --export-width=32 --export-filename=./tmp.png
magick ./tmp.png ./public/favicon.ico
rm ./tmp.png
inkscape --export-type="png" --export-width=140 --export-filename="./tmp.png" ./public/icon.svg
# FFFFF0 matches "ivory" from the CSS
magick ./tmp.png -background "#fffff0" -gravity center -extent 180x180 ./public/apple-touch-icon.png
rm ./tmp.png
```
