# A Practitioner's Decision Tree for Panel-Data Causal Inference

An shiny App-based interactive decision tree that recommends an appropriate estimator for panel data based on the structure of your design (number of treated units, treatment timing, absorbing vs. non-absorbing, binary vs. continuous, ...). Each leaf returns the primary method, alternatives, identifying assumptions, caveats, an R package, and minimal example code.

The app was initially created to provide a heuristic for EUI students to find the most appropriate estimator in the stunningly fast growing literature on DiD-like estimators and extensions. It is a work in progress, so please reach out if you find mistakes or you think something is missing!


**Live:** <https://robertovalli.github.io/practitioner_guide_did/>

## Repository layout

```
app/                      # version-controlled source
  app.R                   # Shiny UI + server
  tree.json               # decision-tree nodes and recommendations
  citations.json          # full reference text per leaf
  generate_citations.R    # rebuilds citations.json from a .bib file
  generate_mermaid.R      # rebuilds the static mermaid diagram (PDF fallback)
index.html                # GitHub Pages entry point (generated)
app.json                  # bundled app source (generated)
shinylive/                # WebAssembly + JS assets (generated)
shinylive-sw.js           # service worker (generated)
```

The `index.html`, `app.json`, `shinylive/`, and `shinylive-sw.js` files at
the root are the output of `shinylive::export()`. They are committed because
GitHub Pages serves the repo root directly.

## Rebuilding the app

After editing anything in `app/`:

```r
# from the repo root
shinylive::export(
  appdir  = "/tmp/shinylive_stage",   # any path OUTSIDE this repo
  destdir = "."
)
```

The two paths must not overlap; if `appdir` is nested inside `destdir`
shinylive silently produces an empty `app.json`. The simplest workflow is to
copy `app/*.R` and `app/*.json` to a temp directory first, then export.

Preview locally before committing:

```sh
python3 -m http.server 8765
# then open http://127.0.0.1:8765/
```

Opening `index.html` directly via `file://` will fail because of ES-module
loading restrictions; a real HTTP server is required.

## Deployment

GitHub Pages is configured to serve from `main` branch, root folder. Pushing
to `main` updates the live site within a minute or two.

## License

See [LICENSE](LICENSE).
